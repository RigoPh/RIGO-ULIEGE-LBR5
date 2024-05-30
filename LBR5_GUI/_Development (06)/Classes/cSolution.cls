VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cSolution"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
#Const DebugMode = 0
#If DebugMode Then
    Private mlClassDebugID As Long
#End If
Public Enum Thickness
    Net = 0
    Gross = 1
End Enum
Private mvarIsSolution As Boolean
Private mvarShowSolution As Boolean
Private mvarShowUpdatedScantling As Boolean
Private mvarShowVariation As Boolean
Private mvarCurrentLoadCase As Integer
Private mvarCurrentSection As Integer
Private mvarCurrentDiagram As Integer
Private mvarDiagramScale As Double
Private mvarShowNeutralAxis As Boolean
Private mvarShowLegend As Boolean
Private mvarNeutralAxis As Double ' if IANA = 2 then NeutralAxis - Net
Private mvarNeutralAxisGross As Double
Private mvarIyy As Double 'if IANA = 2 then Iyy - Net
Private mvarIyyGross As Double

Private mvarThickness As Thickness
' Colors
Private mvarColor1 As ColorConstants
Private mvarColor2 As ColorConstants
Private mvarColor3 As ColorConstants
Private mvarColor4 As ColorConstants
Private mvarColor5 As ColorConstants
Private mvarColor6 As ColorConstants
Private mvarColor7 As ColorConstants
Private mvarColor8 As ColorConstants
Private mvarColor9 As ColorConstants
Private mvarColor10 As ColorConstants
Private mvarColor11 As ColorConstants
Private mvarColor12 As ColorConstants
Private mvarColor13 As ColorConstants
Private mvarColor14 As ColorConstants
Private mvarColor15 As ColorConstants
Private mvarColor16 As ColorConstants

Private mvarSolution As Variant

Public Property Let IsSolution(ByVal vData As Boolean)
    On Error GoTo IsSolutionLetErr
    mvarIsSolution = vData
    Exit Property
IsSolutionLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: IsSolution Property Let")
End Property

Public Property Get IsSolution() As Boolean
    On Error GoTo IsSolutionGetErr
    IsSolution = mvarIsSolution
    Exit Property
IsSolutionGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: IsSolution Property Get")
End Property

Public Property Let ShowSolution(ByVal vData As Boolean)
    On Error GoTo ShowSolutionLetErr
    mvarShowSolution = vData
    Exit Property
ShowSolutionLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: ShowSolution Property Let")
End Property

Public Property Get ShowSolution() As Boolean
    On Error GoTo ShowSolutionGetErr
    ShowSolution = mvarShowSolution
    Exit Property
ShowSolutionGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: ShowSolution Property Get")
End Property

Public Property Get ShowUpdatedScantling() As Boolean
    On Error GoTo ShowUpdatedScantlingGetErr
        ShowUpdatedScantling = mvarShowUpdatedScantling
    Exit Property
ShowUpdatedScantlingGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: ShowUpdatedScantling Property Get")
End Property

Public Property Let ShowUpdatedScantling(ByVal vData As Boolean)
    On Error GoTo ShowUpdatedScantlingLetErr
        mvarShowUpdatedScantling = vData
    Exit Property
ShowUpdatedScantlingLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: ShowUpdatedScantling Property Let")
End Property

Public Property Get ShowVariation() As Boolean
    On Error GoTo ShowVariationGetErr
        ShowVariation = mvarShowVariation
    Exit Property
ShowVariationGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: ShowVariation Property Get")
End Property

Public Property Let ShowVariation(ByVal vData As Boolean)
    On Error GoTo ShowVariationLetErr
        mvarShowVariation = vData
    Exit Property
ShowVariationLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: ShowVariation Property Let")
End Property

Public Property Let CurrentLoadCase(ByVal vData As Integer)
    On Error GoTo CurrentLoadCaseLetErr
    mvarCurrentLoadCase = vData
    Exit Property
CurrentLoadCaseLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: CurrentLoadCase Property Let")
End Property

Public Property Get CurrentLoadCase() As Integer
    On Error GoTo CurrentLoadCaseGetErr
    CurrentLoadCase = mvarCurrentLoadCase
    Exit Property
CurrentLoadCaseGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: CurrentLoadCase Property Get")
End Property

Public Property Let CurrentSection(ByVal vData As Integer)
    On Error GoTo CurrentSectionLetErr
    mvarCurrentSection = vData
    Exit Property
CurrentSectionLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: CurrentSection Property Let")
End Property

Public Property Get CurrentSection() As Integer
    On Error GoTo CurrentSectionGetErr
    CurrentSection = mvarCurrentSection
    Exit Property
CurrentSectionGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: CurrentSection Property Get")
End Property

Public Property Let CurrentDiagram(ByVal vData As Integer)
    On Error GoTo CurrentDiagramLetErr
    mvarCurrentDiagram = vData
    Exit Property
CurrentDiagramLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: CurrentDiagram Property Let")
End Property

Public Property Get CurrentDiagram() As Integer
    On Error GoTo CurrentDiagramGetErr
    CurrentDiagram = mvarCurrentDiagram
    Exit Property
CurrentDiagramGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: CurrentDiagram Property Get")
End Property

Public Property Let DiagramScale(ByVal vData As Double)
    On Error GoTo DiagramScaleLetErr
    mvarDiagramScale = vData
    Exit Property
DiagramScaleLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: DiagramScale Property Let")
End Property

Public Property Get DiagramScale() As Double
    On Error GoTo DiagramScaleGetErr
    DiagramScale = mvarDiagramScale
    Exit Property
DiagramScaleGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: DiagramScale Property Get")
End Property

Public Property Let ShowNeutralAxis(ByVal vData As Boolean)
    On Error GoTo ShowNeutralAxisLetErr
    mvarShowNeutralAxis = vData
    Exit Property
ShowNeutralAxisLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: ShowNeutralAxis Property Let")
End Property

Public Property Get ShowNeutralAxis() As Boolean
    On Error GoTo ShowNeutralAxisGetErr
    ShowNeutralAxis = mvarShowNeutralAxis
    Exit Property
ShowNeutralAxisGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: ShowNeutralAxis Property Get")
End Property

Public Property Let ShowLegend(ByVal vData As Boolean)
    On Error GoTo ShowLegendLetErr
    mvarShowLegend = vData
    Exit Property
ShowLegendLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: ShowLegend Property Let")
End Property

Public Property Get ShowLegend() As Boolean
    On Error GoTo ShowLegendGetErr
    ShowLegend = mvarShowLegend
    Exit Property
ShowLegendGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: ShowLegend Property Get")
End Property

Public Property Let NeutralAxis(ByVal vData As Double)
    On Error GoTo NeutralAxisLetErr
    mvarNeutralAxis = vData
    Exit Property
NeutralAxisLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: NeutralAxis Property Let")
End Property

Public Property Get NeutralAxis() As Double
    On Error GoTo NeutralAxisGetErr
    NeutralAxis = mvarNeutralAxis
    Exit Property
NeutralAxisGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: NeutralAxis Property Get")
End Property

Public Property Let NeutralAxisGross(ByVal vData As Double)
    On Error GoTo NeutralAxisGrossLetErr
    mvarNeutralAxisGross = vData
    Exit Property
NeutralAxisGrossLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: NeutralAxisGross Property Let")
End Property

Public Property Get NeutralAxisGross() As Double
    On Error GoTo NeutralAxisGrossGetErr
    NeutralAxisGross = mvarNeutralAxisGross
    Exit Property
NeutralAxisGrossGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: NeutralAxisGross Property Get")
End Property

Public Property Let Iyy(ByVal vData As Double)
    On Error GoTo IyyLetErr
    mvarIyy = vData
    Exit Property
IyyLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Iyy Property Let")
End Property

Public Property Get Iyy() As Double
    On Error GoTo IyyGetErr
    Iyy = mvarIyy
    Exit Property
IyyGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Iyy Property Get")
End Property

Public Property Let IyyGross(ByVal vData As Double)
    On Error GoTo IyyGrossLetErr
    mvarIyyGross = vData
    Exit Property
IyyGrossLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: IyyGross Property Let")
End Property

Public Property Get IyyGross() As Double
    On Error GoTo IyyGrossGetErr
    IyyGross = mvarIyyGross
    Exit Property
IyyGrossGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: IyyGross Property Get")
End Property


Public Property Let Thickness(ByVal vData As Thickness)
    On Error GoTo ThicknessLetErr
    mvarThickness = vData
    Exit Property
ThicknessLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Thickness Property Let")
End Property

Public Property Get Thickness() As Thickness
    On Error GoTo ThicknessGetErr
    Thickness = mvarThickness
    Exit Property
ThicknessGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Thickness Property Get")
End Property

Public Property Let Color1(ByVal vData As ColorConstants)
    On Error GoTo Color1LetErr
    mvarColor1 = vData
    Exit Property
Color1LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color1 Property Let")
End Property

Public Property Get Color1() As ColorConstants
    On Error GoTo Color1GetErr
    Color1 = mvarColor1
    Exit Property
Color1GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color1 Property Get")
End Property

Public Property Let Color2(ByVal vData As ColorConstants)
    On Error GoTo Color2LetErr
    mvarColor2 = vData
    Exit Property
Color2LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color2 Property Let")
End Property

Public Property Get Color2() As ColorConstants
    On Error GoTo Color2GetErr
    Color2 = mvarColor2
    Exit Property
Color2GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color2 Property Get")
End Property

Public Property Let Color3(ByVal vData As ColorConstants)
    On Error GoTo Color3LetErr
    mvarColor3 = vData
    Exit Property
Color3LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color3 Property Let")
End Property

Public Property Get Color3() As ColorConstants
    On Error GoTo Color3GetErr
    Color3 = mvarColor3
    Exit Property
Color3GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color3 Property Get")
End Property

Public Property Let Color4(ByVal vData As ColorConstants)
    On Error GoTo Color4LetErr
    mvarColor4 = vData
    Exit Property
Color4LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color4 Property Let")
End Property

Public Property Get Color4() As ColorConstants
    On Error GoTo Color4GetErr
    Color4 = mvarColor4
    Exit Property
Color4GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color4 Property Get")
End Property

Public Property Let Color5(ByVal vData As ColorConstants)
    On Error GoTo Color5LetErr
    mvarColor5 = vData
    Exit Property
Color5LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color5 Property Let")
End Property

Public Property Get Color5() As ColorConstants
    On Error GoTo Color5GetErr
    Color5 = mvarColor5
    Exit Property
Color5GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color5 Property Get")
End Property

Public Property Let Color6(ByVal vData As ColorConstants)
    On Error GoTo Color6LetErr
    mvarColor6 = vData
    Exit Property
Color6LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color6 Property Let")
End Property

Public Property Get Color6() As ColorConstants
    On Error GoTo Color6GetErr
    Color6 = mvarColor6
    Exit Property
Color6GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color6 Property Get")
End Property

Public Property Let Color7(ByVal vData As ColorConstants)
    On Error GoTo Color7LetErr
    mvarColor7 = vData
    Exit Property
Color7LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color7 Property Let")
End Property

Public Property Get Color7() As ColorConstants
    On Error GoTo Color7GetErr
    Color7 = mvarColor7
    Exit Property
Color7GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color7 Property Get")
End Property

Public Property Let Color8(ByVal vData As ColorConstants)
    On Error GoTo Color8LetErr
    mvarColor8 = vData
    Exit Property
Color8LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color8 Property Let")
End Property

Public Property Get Color8() As ColorConstants
    On Error GoTo Color8GetErr
    Color8 = mvarColor8
    Exit Property
Color8GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color8 Property Get")
End Property

Public Property Let Color9(ByVal vData As ColorConstants)
    On Error GoTo Color9LetErr
    mvarColor9 = vData
    Exit Property
Color9LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color9 Property Let")
End Property

Public Property Get Color9() As ColorConstants
    On Error GoTo Color9GetErr
    Color9 = mvarColor9
    Exit Property
Color9GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color9 Property Get")
End Property

Public Property Let Color10(ByVal vData As ColorConstants)
    On Error GoTo Color10LetErr
    mvarColor10 = vData
    Exit Property
Color10LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color10 Property Let")
End Property

Public Property Get Color10() As ColorConstants
    On Error GoTo Color10GetErr
    Color10 = mvarColor10
    Exit Property
Color10GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color10 Property Get")
End Property

Public Property Let Color11(ByVal vData As ColorConstants)
    On Error GoTo Color11LetErr
    mvarColor11 = vData
    Exit Property
Color11LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color11 Property Let")
End Property

Public Property Get Color11() As ColorConstants
    On Error GoTo Color11GetErr
    Color11 = mvarColor11
    Exit Property
Color11GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color11 Property Get")
End Property

Public Property Let Color12(ByVal vData As ColorConstants)
    On Error GoTo Color12LetErr
    mvarColor12 = vData
    Exit Property
Color12LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color12 Property Let")
End Property

Public Property Get Color12() As ColorConstants
    On Error GoTo Color12GetErr
    Color12 = mvarColor12
    Exit Property
Color12GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color12 Property Get")
End Property

Public Property Let Color13(ByVal vData As ColorConstants)
    On Error GoTo Color13LetErr
    mvarColor13 = vData
    Exit Property
Color13LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color13 Property Let")
End Property

Public Property Get Color13() As ColorConstants
    On Error GoTo Color13GetErr
    Color13 = mvarColor13
    Exit Property
Color13GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color13 Property Get")
End Property

Public Property Let Color14(ByVal vData As ColorConstants)
    On Error GoTo Color14LetErr
    mvarColor14 = vData
    Exit Property
Color14LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color14 Property Let")
End Property

Public Property Get Color14() As ColorConstants
    On Error GoTo Color14GetErr
    Color14 = mvarColor14
    Exit Property
Color14GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color14 Property Get")
End Property

Public Property Let Color15(ByVal vData As ColorConstants)
    On Error GoTo Color15LetErr
    mvarColor15 = vData
    Exit Property
Color15LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color15 Property Let")
End Property

Public Property Get Color15() As ColorConstants
    On Error GoTo Color15GetErr
    Color15 = mvarColor15
    Exit Property
Color15GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color15 Property Get")
End Property

Public Property Let Color16(ByVal vData As ColorConstants)
    On Error GoTo Color16LetErr
    mvarColor16 = vData
    Exit Property
Color16LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color16 Property Let")
End Property

Public Property Get Color16() As ColorConstants
    On Error GoTo Color16GetErr
    Color16 = mvarColor16
    Exit Property
Color16GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Color16 Property Get")
End Property

Public Property Let Solution(ByVal vData As Variant)
    On Error GoTo SolutionLetErr
    mvarSolution = vData
    Exit Property
SolutionLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Solution Property Let")
End Property

Public Property Get Solution() As Variant
    On Error GoTo SolutionGetErr
    Solution = mvarSolution
    Exit Property
SolutionGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Solution Property Get")
End Property

Public Function GetColorMatrix(ByVal index As Integer) As ColorConstants
    Select Case index
        Case 1
            GetColorMatrix = Me.Color1
        Case 2
            GetColorMatrix = Me.Color2
        Case 3
            GetColorMatrix = Me.Color3
        Case 4
            GetColorMatrix = Me.Color4
        Case 5
            GetColorMatrix = Me.Color5
        Case 6
            GetColorMatrix = Me.Color6
        Case 7
            GetColorMatrix = Me.Color7
        Case 8
            GetColorMatrix = Me.Color8
        Case 9
            GetColorMatrix = Me.Color9
        Case 10
            GetColorMatrix = Me.Color10
        Case 11
            GetColorMatrix = Me.Color11
        Case 12
            GetColorMatrix = Me.Color12
        Case 13
            GetColorMatrix = Me.Color13
        Case 14
            GetColorMatrix = Me.Color14
        Case 15
            GetColorMatrix = Me.Color15
        Case 16
            GetColorMatrix = Me.Color16
    End Select
End Function

Public Function Clone() As cSolution
    On Error GoTo CloneErr
    Set Clone = New cSolution
    Clone.IsSolution = Me.IsSolution
    Clone.ShowSolution = Me.ShowSolution
    Clone.CurrentLoadCase = Me.CurrentLoadCase
    Clone.CurrentSection = Me.CurrentSection
    Clone.CurrentDiagram = Me.CurrentDiagram
    Clone.DiagramScale = Me.DiagramScale
    Clone.ShowNeutralAxis = Me.ShowNeutralAxis
    Clone.ShowLegend = Me.ShowLegend
    Clone.ShowUpdatedScantling = Me.ShowUpdatedScantling
    Clone.ShowVariation = Me.ShowVariation
    Clone.Thickness = Me.Thickness
    Clone.NeutralAxis = Me.NeutralAxis
    Clone.NeutralAxisGross = Me.NeutralAxisGross
    Clone.Iyy = Me.Iyy
    Clone.IyyGross = Me.IyyGross
    ' Colors
    Clone.Color1 = Me.Color1
    Clone.Color2 = Me.Color2
    Clone.Color3 = Me.Color3
    Clone.Color4 = Me.Color4
    Clone.Color5 = Me.Color5
    Clone.Color6 = Me.Color6
    Clone.Color7 = Me.Color7
    Clone.Color8 = Me.Color8
    Clone.Color9 = Me.Color9
    Clone.Color10 = Me.Color10
    Clone.Color11 = Me.Color11
    Clone.Color12 = Me.Color12
    Clone.Color13 = Me.Color13
    Clone.Color14 = Me.Color14
    Clone.Color15 = Me.Color15
    Clone.Color16 = Me.Color16
    Clone.Solution = Me.Solution
    Clone.Solution = Me.Solution
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim vv() As Variant
    With Me
        Put #iNoOfFile, , CBool(.IsSolution)
        If .IsSolution = True Then
            vv = .Solution
            Put #iNoOfFile, , vv
            Put #iNoOfFile, , .NeutralAxis
            Put #iNoOfFile, , .NeutralAxisGross
            Put #iNoOfFile, , .Iyy
            Put #iNoOfFile, , .IyyGross
        End If
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim vv() As Variant, BB As Boolean
    With Me
        Get #iNoOfFile, , BB: .IsSolution = BB
        If .IsSolution = True Then
            SolMatrix lProjectCount
            Get #iNoOfFile, , POL: .Solution = POL
        End If
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSolution: Function ReadASCII")
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
