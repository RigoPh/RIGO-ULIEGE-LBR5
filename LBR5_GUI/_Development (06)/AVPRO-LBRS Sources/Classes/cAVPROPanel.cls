VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cAVPROPanel"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit

' **************************************
'   cAVPROPanel
'
'   en conformité avec
'   le schéma AVPROLBR5.xsd du 17/10/2003
'
'   NBD/PRINCIPIA MARINE-17/10/2003
' **************************************

Public Enum enmSide
    NEGATIVE_SIDE
    POSITIVE_SIDE
End Enum

Public Enum enmStiffSetType
    PRIMARYSET
    SECUNDARYSET
End Enum

Private sSideLabelTable(2) As String
Private sStiffSetTypeLabelTable(2) As String

Private m_Index             As Integer
Private m_Surface           As Integer
Private m_StartPoint        As New cAVPROPoint
Private m_EndPoint          As New cAVPROPoint
Private m_Length            As Double
Private m_Angle             As Double
Private m_Thickness         As Double
Private m_SideFrame         As enmSide
Private m_Frame             As New cAVPROStiffener
Private m_SideStiff         As enmSide
Private m_RepartitionMode   As Integer
Private m_Stiffener         As New cAVPROStiffener
Private m_Material          As New cAVPROMaterial
Private m_SideGirder        As enmSide
Public m_LongiGirders      As New Collection
'FIXIT: Declare 'm_ConnectedPanel' with an early-bound data type                           FixIT90210ae-R1672-R1B8ZE
Private m_ConnectedPanel()  As Variant
'FIXIT: Declare 'm_Boundary' with an early-bound data type                                 FixIT90210ae-R1672-R1B8ZE
Private m_Boundary()        As Variant
Private m_SidePressure      As enmSide
Public m_PressureLoads     As New Collection

'FIXIT: Declare 'm_BoundaryConditionLabels' with an early-bound data type                  FixIT90210ae-R1672-R1B8ZE
Private m_BoundaryConditionLabels
'FIXIT: Declare 'm_SurfaceLabels' with an early-bound data type                            FixIT90210ae-R1672-R1B8ZE
Private m_SurfaceLabels
'FIXIT: Declare 'm_RepartitionModeLabels' with an early-bound data type                    FixIT90210ae-R1672-R1B8ZE
Private m_RepartitionModeLabels

Private Const PRESSURELOADSTRINGKEY As String = "PressureLoad"
Private Const GIRDERSTRINGKEY As String = "Girder"


' *****************************
' ***** PUBLIC PROPERTIES *****
' *****************************

'FIXIT: Declare 'value' with an early-bound data type                                      FixIT90210ae-R1672-R1B8ZE
Public Property Let ConnectedPanel(ByVal Value As Variant)
m_ConnectedPanel = ConnectedPanel
End Property

'FIXIT: Declare 'ConnectedPanel' with an early-bound data type                             FixIT90210ae-R1672-R1B8ZE
Public Property Get ConnectedPanel() As Variant
ConnectedPanel = m_ConnectedPanel
End Property

'FIXIT: Declare 'value' with an early-bound data type                                      FixIT90210ae-R1672-R1B8ZE
Public Property Let Boundary(ByVal Value As Variant)
m_Boundary = Boundary
End Property

'FIXIT: Declare 'Boundary' with an early-bound data type                                   FixIT90210ae-R1672-R1B8ZE
Public Property Get Boundary() As Variant
Boundary = m_Boundary
End Property

Public Property Let index(ByVal Value As Integer)
    m_Index = Value
End Property

Public Property Get index() As Integer
    index = m_Index
End Property

Public Property Let Surface(ByVal Value As Integer)
    If Value > 0 And Value <= m_SurfaceLabels.Count Then
        m_Surface = Value
    Else
        MsgBox "Error : cAVPROPanel.Surface : unknown surface !"
    End If
End Property

Public Property Get Surface() As Integer
    Surface = m_Surface
End Property

Public Property Get SurfaceLabel() As String
    SurfaceLabel = m_SurfaceLabels(m_Surface)
End Property

Public Property Let StartPoint(ByVal Value As cAVPROPoint)
    Set m_StartPoint = Value
End Property

Public Property Get StartPoint() As cAVPROPoint
    Set StartPoint = m_StartPoint
End Property

Public Property Let EndPoint(ByVal Value As cAVPROPoint)
    Set m_EndPoint = Value
End Property

Public Property Get EndPoint() As cAVPROPoint
    Set EndPoint = m_EndPoint
End Property

Public Property Let length(ByVal Value As Double)
    m_Length = Value
End Property

Public Property Get length() As Double
    length = m_Length
End Property

Public Property Let angle(ByVal Value As Double)
    m_Angle = Value
End Property

Public Property Get angle() As Double
    angle = m_Angle
End Property


Public Property Let Thickness(ByVal Value As Double)
    m_Thickness = Value
End Property

Public Property Get Thickness() As Double
    Thickness = m_Thickness
End Property

Public Property Let SideFrame(ByVal Value As enmSide)
    m_SideFrame = Value
End Property

Public Property Get SideFrame() As enmSide
    SideFrame = m_SideFrame
End Property

Public Property Let SideStiffener(ByVal Value As enmSide)
    m_SideStiff = Value
End Property

Public Property Get SideStiffener() As enmSide
    SideStiffener = m_SideStiff
End Property

Public Property Let Frame(ByVal Value As cAVPROStiffener)
    Set m_Frame = Value
End Property

Public Property Get Frame() As cAVPROStiffener
    Set Frame = m_Frame
End Property

Public Property Let Stiffener(ByVal Value As cAVPROStiffener)
    Set m_Stiffener = Value
End Property

Public Property Get Stiffener() As cAVPROStiffener
    Set Stiffener = m_Stiffener
End Property

Public Property Let RepartitionMode(ByVal Value As Integer)
    If Value > 0 And Value <= m_RepartitionModeLabels.Count Then
        m_RepartitionMode = Value
    Else
        MsgBox "Error : cAVPROPanel.RepartitionMode : unknown repartition mode !"
    End If
End Property

Public Property Get RepartitionMode() As Integer
    RepartitionMode = m_RepartitionMode
End Property

Public Property Get RepartitionModeLabel() As String
    RepartitionModeLabel = m_RepartitionModeLabels(m_RepartitionMode)
End Property

Public Property Let Material(ByVal Value As cAVPROMaterial)
    Set m_Material = Value
End Property

Public Property Get Material() As cAVPROMaterial
    Set Material = m_Material
End Property

Public Property Let SidePressure(ByVal Value As enmSide)
    m_SidePressure = Value
End Property

Public Property Get SidePressure() As enmSide
    SidePressure = m_SidePressure
End Property

Public Property Let SideGirder(ByVal Value As enmSide)
    m_SideGirder = Value
End Property

Public Property Get SideGirder() As enmSide
    SideGirder = m_SideGirder
End Property


' *****************************
' ****** PRIVATE METHODS ******
' *****************************

Private Sub Class_Initialize()
    sSideLabelTable(NEGATIVE_SIDE) = "NEGATIVE_SIDE"
    sSideLabelTable(POSITIVE_SIDE) = "POSITIVE_SIDE"
    
    sStiffSetTypeLabelTable(PRIMARYSET) = "Primary"
    sStiffSetTypeLabelTable(SECUNDARYSET) = "Secundary"
    
    Set m_BoundaryConditionLabels = CreateObject("Scripting.Dictionary")
    m_BoundaryConditionLabels.Add 1, "Free edge"
    m_BoundaryConditionLabels.Add 2, "Simply supported #2"
    m_BoundaryConditionLabels.Add 3, "Simply supported #3"
    m_BoundaryConditionLabels.Add 4, "Simply supported #4"
    m_BoundaryConditionLabels.Add 5, "Simply supported #5"
    m_BoundaryConditionLabels.Add 6, "Clamped"
    m_BoundaryConditionLabels.Add 7, "Symmetry axis #1"
    m_BoundaryConditionLabels.Add 8, "Simply supported #6"
    m_BoundaryConditionLabels.Add 9, "Simply supported #7"
    m_BoundaryConditionLabels.Add 10, "Simply supported #8"
    m_BoundaryConditionLabels.Add 11, "Symmetry axis #2"
    m_BoundaryConditionLabels.Add 12, "Double symmetry"

    Set m_SurfaceLabels = CreateObject("Scripting.Dictionary")
    m_SurfaceLabels.Add 1, "Keel Plate"
    m_SurfaceLabels.Add 2, "Bottom"
    m_SurfaceLabels.Add 3, "Side"
    m_SurfaceLabels.Add 4, "Deck"
    m_SurfaceLabels.Add 5, "Main Deck"
    m_SurfaceLabels.Add 6, "Inner Deck"
    m_SurfaceLabels.Add 7, "Superstructure Deck"
    m_SurfaceLabels.Add 8, "Longitudinal Bulkhead"
    '' A COMPLETER...
    
    Set m_RepartitionModeLabels = CreateObject("Scripting.Dictionary")
    m_RepartitionModeLabels.Add 1, "EE1"
    m_RepartitionModeLabels.Add 2, "EE2"

    ReDim m_ConnectedPanel(0)
    ReDim m_Boundary(0)
    ReDim m_longigirder(0)
End Sub


' *****************************
' ****** PUBLIC METHODS *******
' *****************************

Public Function GetRepartitionModeIndex(sModeName As String) As Integer
    Dim i As Integer
    Dim bIndexFound As Boolean
    
    For i = 1 To m_RepartitionModeLabels.Count
        If m_RepartitionModeLabels(i) = sModeName Then
            GetRepartitionModeIndex = i
            bIndexFound = True
            Exit For
        End If
    Next i
    
    If Not bIndexFound Then MsgBox "Error : cAVPROPanel.GetRepartitionModeIndex : unknown mode !", vbCritical + vbOKOnly
    
End Function

Public Function GetSurfaceIndex(sSurfaceName As String) As Integer
    Dim i As Integer
    Dim bIndexFound As Boolean
    
    For i = 1 To m_SurfaceLabels.Count
        If m_SurfaceLabels(i) = sSurfaceName Then
            GetSurfaceIndex = i
            bIndexFound = True
            Exit For
        End If
    Next i
    
    If Not bIndexFound Then MsgBox "Error : cAVPROPanel.GetSurfaceIndex : unknown surface !", vbCritical + vbOKOnly
    
End Function

Public Function GetBoundaryConditionIndex(sConditionName As String) As Integer
    Dim i As Integer
    Dim bIndexFound As Boolean
    
    For i = 1 To m_BoundaryConditionLabels.Count
        If m_BoundaryConditionLabels(i) = sConditionName Then
            GetBoundaryConditionIndex = i
            bIndexFound = True
            Exit For
        End If
    Next i
    
    If Not bIndexFound Then MsgBox "Error : cAVPROPanel.GetBoundaryConditionIndex : unknown condition !", vbCritical + vbOKOnly
    
End Function

Public Sub AddAPressureLoad(oPressureLoad As cAVPROPressureLoad)
On Error GoTo ErrHandler
    m_PressureLoads.Add oPressureLoad, PRESSURELOADSTRINGKEY & oPressureLoad.index
    Exit Sub
ErrHandler:
    MsgBox "Error : cAVPROPanel.AddAPressureLoad : pressure load " & oPressureLoad.index & " already exists !", vbCritical + vbOKOnly
End Sub

Public Function GetPressureLoad(iIndex As Integer) As cAVPROLoadCase
'FIXIT: Declare 'oPressureLoad' with an early-bound data type                              FixIT90210ae-R1672-R1B8ZE
    Dim oPressureLoad As Object
    Dim i As Integer
    
    For i = 1 To m_PressureLoads.Count
        If m_PressureLoads(i).index = iIndex Then
            Set oPressureLoad = m_PressureLoads.Item(i)
            Exit For
        End If
    Next i
    
    Set GetPressureLoad = oPressureLoad
    
End Function

Public Function RemoveAPressureLoad(iIndex As Integer) As Boolean
    Dim i As Integer
    
    For i = 1 To m_PressureLoads.Count
        If m_PressureLoads(i).index = iIndex Then
            m_PressureLoads.Remove i
            RemoveAPressureLoad = True
            Exit For
        End If
    Next i
End Function

'FIXIT: Declare 'getConnectedPanel' with an early-bound data type                          FixIT90210ae-R1672-R1B8ZE
Public Function getConnectedPanel(index As Integer) As Variant
getConnectedPanel = m_ConnectedPanel(index)
End Function


Public Function NoOfConnectedPanels() As Integer
    NoOfConnectedPanels = UBound(m_ConnectedPanel)
End Function

Public Sub AddAConnectedPanel(iPanelIndex As Integer)
    ReDim Preserve m_ConnectedPanel(NoOfConnectedPanels + 1)
    m_ConnectedPanel(NoOfConnectedPanels) = iPanelIndex
End Sub

Public Function RemoveAConnectedPanel(iPanelIndex As Integer) As Boolean
    Dim i As Integer
    Dim bPanelFound As Boolean
    
    For i = 1 To NoOfConnectedPanels
        If bPanelFound Then
            m_ConnectedPanel(i - 1) = m_ConnectedPanel(i)
        Else
            If m_ConnectedPanel(i) = iPanelIndex Then
                bPanelFound = True
            End If
        End If
    Next i
    If bPanelFound Then
        ReDim Preserve m_ConnectedPanel(NoOfConnectedPanels - 1)
        RemoveAConnectedPanel = True
    End If
End Function

Public Function NoOfBoundaryConditions() As Integer
    NoOfBoundaryConditions = UBound(m_Boundary)
End Function

Public Sub AddABoundaryCondition(iCondition As Integer)
If NoOfBoundaryConditions < 2 Then
    ReDim Preserve m_Boundary(NoOfBoundaryConditions + 1)
    If iCondition > 0 And iCondition <= m_BoundaryConditionLabels.Count Then
        m_Boundary(NoOfBoundaryConditions) = iCondition
    Else
        MsgBox "ERROR : cAVPROPanel.AddABoundaryCondition : this boundary condition is not supported !", vbCritical + vbOKOnly
    End If
Else
    MsgBox "ERROR : cAVPROPanel.AddABoundaryCondition : no more than 2 boundary conditions can be defined !", vbCritical + vbOKOnly
End If
End Sub

Public Function RemoveABoundaryCondition(sCondition As String) As Boolean
    Dim i As Integer
    Dim bConditionFound As Boolean

    For i = 1 To NoOfBoundaryConditions
        If bConditionFound Then
            m_Boundary(i - 1) = m_Boundary(i)
        Else
            If m_Boundary(i) = sCondition Then
                bConditionFound = True
            End If
        End If
    Next i
    If bConditionFound Then
        ReDim Preserve m_Boundary(NoOfBoundaryConditions - 1)
        RemoveABoundaryCondition = True
    End If
End Function

Public Function NoOfGirders() As Integer
    NoOfGirders = m_LongiGirders.Count
End Function

Public Sub AddAGirder(oGirder As cAVPROGirder)
On Error GoTo ErrHandler
    m_LongiGirders.Add oGirder, GIRDERSTRINGKEY & oGirder.Position
    Exit Sub
ErrHandler:
    MsgBox "Error : cAVPROPanel.AddAGirder : girder at " & oGirder.Position & " already exists !", vbCritical + vbOKOnly
End Sub

Public Function RemoveAGirder(dPosition As Double) As Boolean
    Dim i As Integer
    Dim bGirderFound As Boolean

    For i = 1 To m_LongiGirders.Count
        If m_LongiGirders(i).Position = dPosition Then
            m_LongiGirders.Remove i
            bGirderFound = True
            Exit For
        End If
    Next i
    If bGirderFound Then
        RemoveAGirder = True
    End If
End Function

Public Sub writeToXML(file As Integer, Optional tabulation As Integer = 0)
    Dim i As Integer
    Dim spaces As Integer
    spaces = tabulation * 3

'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "<PANEL Index=" & Chr$(34) & m_Index & Chr$(34) & " Surface=" & Chr$(34) & m_SurfaceLabels(m_Surface) & Chr$(34) & ">"
    
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "   <GEOMETRY>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "      <END_POINTS>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "         <START_POINT>"
    m_StartPoint.writeToXML file, tabulation + 4
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "         </START_POINT>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "         <END_POINT>"
    m_EndPoint.writeToXML file, tabulation + 3
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "         </END_POINT>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "      </END_POINTS>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "      <LENGTH>" & Format(m_Length, "0.0000") & "</LENGTH>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "      <ANGLE>" & Format(m_Angle, "0.0000") & "</ANGLE>"
    If UBound(m_ConnectedPanel) > 0 Then
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
        Print #file, Spc(spaces); "      <CONNECTED_PANELS>"
        For i = 1 To UBound(m_ConnectedPanel)
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
            Print #file, Spc(spaces); "         <INDEX>" & m_ConnectedPanel(i) & "</INDEX>"
        Next i
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
        Print #file, Spc(spaces); "      </CONNECTED_PANELS>"
    End If
    If NoOfBoundaryConditions > 0 Then
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
        Print #file, Spc(spaces); "      <BOUNDARIES>"
        For i = 1 To UBound(m_Boundary)
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
            Print #file, Spc(spaces); "         <BOUNDARY>" & m_BoundaryConditionLabels(m_Boundary(i)) & "</BOUNDARY>"
        Next i
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
        Print #file, Spc(spaces); "      </BOUNDARIES>"
    End If
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "   </GEOMETRY>"
    
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "   <SCANTLINGS>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "      <THICKNESS>" & m_Thickness & "</THICKNESS>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "      <FRAMES>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "         <SIDE>" & sSideLabelTable(m_SideFrame) & "</SIDE>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "         <FRAME SetType=" & Chr$(34) & sStiffSetTypeLabelTable(PRIMARYSET) & Chr$(34) & ">"
    m_Frame.writeToXML file, tabulation + 4
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "         </FRAME>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "      </FRAMES>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "      <STIFFENERS>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "         <SIDE>" & sSideLabelTable(m_SideStiff) & "</SIDE>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "         <REPARTITION_MODE>" & m_RepartitionModeLabels(m_RepartitionMode) & "</REPARTITION_MODE>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "         <STIFFENER SetType=" & Chr$(34) & sStiffSetTypeLabelTable(PRIMARYSET) & Chr$(34) & ">"
    m_Stiffener.writeToXML file, tabulation + 4
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "         </STIFFENER>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "      </STIFFENERS>"
    m_Material.writeToXML file, tabulation + 2
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "   </SCANTLINGS>"
    
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "   <PRESSURE_LOADS>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "      <SIDE>" & sSideLabelTable(m_SidePressure) & "</SIDE>"
    For i = 1 To m_PressureLoads.Count
        m_PressureLoads(i).writeToXML file, tabulation + 2
    Next i
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "   </PRESSURE_LOADS>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "</PANEL>"
End Sub

Public Sub readXML(NoOfFile As Integer)
    Dim strLine                 As String
    Dim lineHeader              As String
    Dim attributeStartPosition  As Long
    Dim attributeEndPosition    As Long
    Dim element                 As String
    Dim oGirder                 As cAVPROGirder
    Dim oPressureLoad           As cAVPROPressureLoad

    Do While Not EOF(NoOfFile)
        ' LECTURE DE L'ELEMENT SUIVANT
        Line Input #NoOfFile, strLine
        strLine = cleanLine(strLine)
        lineHeader = readLineHeader(strLine)
        Select Case lineHeader
            Case "GEOMETRY"
                Do
                    Line Input #NoOfFile, strLine
                    strLine = cleanLine(strLine)
                    lineHeader = readLineHeader(strLine)
                    Select Case lineHeader
                        Case "END_POINTS"
                            Do
                                Line Input #NoOfFile, strLine
                                strLine = cleanLine(strLine)
                                lineHeader = readLineHeader(strLine)
                                Select Case lineHeader
                                    Case "START_POINT"
                                        m_StartPoint.readXML NoOfFile
                                    Case "END_POINT"
                                        m_EndPoint.readXML NoOfFile
                                    Case "/END_POINTS"
                                        Exit Do
                                    Case Else
                                        MsgBox "Error : cAVPROPanel.readXML : file bad format !", vbCritical + vbOKOnly
                                End Select
                            Loop
                        Case "LENGTH"
                            m_Length = Val_(readValue(strLine))
                        Case "ANGLE"
                            m_Angle = Val_(readValue(strLine))
                        Case "CONNECTED_PANELS"
                            Do
                                Line Input #NoOfFile, strLine
                                strLine = cleanLine(strLine)
                                lineHeader = readLineHeader(strLine)
                                Select Case lineHeader
                                    Case "INDEX"
                                        AddAConnectedPanel readValue(strLine)
                                    Case "/CONNECTED_PANELS"
                                        Exit Do
                                    Case Else
                                        MsgBox "Error : cAVPROPanel.readXML : file bad format !", vbCritical + vbOKOnly
                                        Exit Do
                                End Select
                            Loop
                        Case "BOUNDARIES"
                            Do
                                Line Input #NoOfFile, strLine
                                strLine = cleanLine(strLine)
                                lineHeader = readLineHeader(strLine)
                                Select Case lineHeader
                                    Case "BOUNDARY"
                                        AddABoundaryCondition GetBoundaryConditionIndex(readValue(strLine))
                                    Case "/BOUNDARIES"
                                        Exit Do
                                    Case Else
                                        MsgBox "Error : cAVPROPanel.readXML : file bad format !", vbCritical + vbOKOnly
                                        Exit Do
                                End Select
                            Loop
                        Case "/GEOMETRY"
                            Exit Do
                        Case Else
                            MsgBox "Error : cAVPROPanel.readXML : file bad format !", vbCritical + vbOKOnly
                            Exit Do
                    End Select
                Loop

            Case "SCANTLINGS"
                Do
                    Line Input #NoOfFile, strLine
                    strLine = cleanLine(strLine)
                    lineHeader = readLineHeader(strLine)
                    Select Case lineHeader
                        Case "THICKNESS"
                            m_Thickness = Val_(readValue(strLine))
                        Case "FRAMES"
                            Do
                                Line Input #NoOfFile, strLine
                                strLine = cleanLine(strLine)
                                lineHeader = readLineHeader(strLine)
                                Select Case lineHeader
                                    Case "SIDE"
                                        If readValue(strLine) = sSideLabelTable(NEGATIVE_SIDE) Then
                                            m_SideFrame = NEGATIVE_SIDE
                                        ElseIf readValue(strLine) = sSideLabelTable(POSITIVE_SIDE) Then
                                            m_SideFrame = POSITIVE_SIDE
                                        Else
                                            MsgBox "Error : cAVPROPanel.readXML : file bad format !", vbCritical + vbOKOnly
                                        End If
                                    Case "FRAME"
                                        m_Frame.readXML NoOfFile
                                    Case "/FRAMES"
                                        Exit Do
                                    Case Else
                                        MsgBox "Error : cAVPROPanel.readXML : file bad format !", vbCritical + vbOKOnly
                                        Exit Do
                                End Select
                            Loop
                        Case "STIFFENERS"
                            Do
                                Line Input #NoOfFile, strLine
                                strLine = cleanLine(strLine)
                                lineHeader = readLineHeader(strLine)
                                Select Case lineHeader
                                    Case "SIDE"
                                        If readValue(strLine) = sSideLabelTable(NEGATIVE_SIDE) Then
                                            m_SideStiff = NEGATIVE_SIDE
                                        ElseIf readValue(strLine) = sSideLabelTable(POSITIVE_SIDE) Then
                                            m_SideStiff = POSITIVE_SIDE
                                        Else
                                            MsgBox "Error : cAVPROPanel.readXML : file bad format !", vbCritical + vbOKOnly
                                        End If
                                    Case "REPARTITION_MODE"
                                        m_RepartitionMode = GetRepartitionModeIndex(readValue(strLine))
                                    Case "STIFFENER"
                                        m_Stiffener.readXML NoOfFile
                                    Case "/STIFFENERS"
                                        Exit Do
                                    Case Else
                                        MsgBox "Error : cAVPROPanel.readXML : file bad format !", vbCritical + vbOKOnly
                                        Exit Do
                                End Select
                            Loop
                            
                        Case "GIRDERS"
                            Do
                                Line Input #NoOfFile, strLine
                                strLine = cleanLine(strLine)
                                lineHeader = readLineHeader(strLine)
                                Select Case lineHeader
                                    Case "SIDE"
                                        If readValue(strLine) = sSideLabelTable(NEGATIVE_SIDE) Then
                                            m_SideGirder = NEGATIVE_SIDE
                                        ElseIf readValue(strLine) = sSideLabelTable(POSITIVE_SIDE) Then
                                            m_SideGirder = POSITIVE_SIDE
                                        Else
                                            MsgBox "Error : cAVPROPanel.readXML : file bad format !", vbCritical + vbOKOnly
                                        End If
                                    Case "GIRDER"
                                        Set oGirder = New cAVPROGirder
                                        oGirder.readXML NoOfFile
                                        AddAGirder oGirder
                                    Case "/GIRDERS"
                                        Exit Do
                                    Case Else
                                        MsgBox "Error : cAVPROPanel.readXML : file bad format !", vbCritical + vbOKOnly
                                        Exit Do
                                End Select
                            Loop
                        Case "MATERIAL"
                            element = readAttribute(strLine, 1) ' Lecture du nom du matériau
                            m_Material.Name = element
                            m_Material.readXML NoOfFile
                            
                        Case "/SCANTLINGS"
                            Exit Do
                        Case Else
                            MsgBox "Error : cAVPROPanel.readXML : file bad format !", vbCritical + vbOKOnly
                            Exit Do
                    End Select
                Loop
                
            Case "PRESSURE_LOADS"
                Do
                    Line Input #NoOfFile, strLine
                    strLine = cleanLine(strLine)
                    lineHeader = readLineHeader(strLine)
                    Select Case lineHeader
                        Case "SIDE"
                            If readValue(strLine) = sSideLabelTable(NEGATIVE_SIDE) Then
                                m_SidePressure = NEGATIVE_SIDE
                            ElseIf readValue(strLine) = sSideLabelTable(POSITIVE_SIDE) Then
                                m_SidePressure = POSITIVE_SIDE
                            Else
                                MsgBox "Error : cAVPROPanel.readXML : file bad format !", vbCritical + vbOKOnly
                            End If
                        Case "PRESSURE_LOAD"
                            Set oPressureLoad = New cAVPROPressureLoad
                            element = readAttribute(strLine, 1) ' Lecture du nom du matériau
                            oPressureLoad.index = element
                            oPressureLoad.readXML NoOfFile
                            AddAPressureLoad oPressureLoad
                        Case "/PRESSURE_LOADS"
                            Exit Do
                        Case Else
                            MsgBox "Error : cAVPROPanel.readXML : file bad format !", vbCritical + vbOKOnly
                            Exit Do
                    End Select
                Loop
                
            Case "GEOMETRY/", "SCANTLINGS/", "PRESSURE_LOADS/"
                MsgBox "Error : cAVPROPanel.readXML : file bad format !", vbCritical + vbOKOnly
            Case "/PANEL"
                Exit Do
            Case "!--"
                ' ligne de commentaires
            Case Else
                MsgBox "Error : cAVPROPanel.readXML : file bad format !", vbCritical + vbOKOnly
                Exit Do
        End Select
    Loop
        
End Sub


' ***************************************
' *********** PRIVATE METHODS ***********
' ***************************************

Private Function readAttribute(strLine As String, noOfAttribute As Integer) As String
    ' RENVOIE L ATTRIBUT NUMERO noOfAttribute
    Dim i As Integer
    Dim strAttribute As String
    Dim attributeStartPosition As Long
    Dim attributeEndPosition As Long

    attributeEndPosition = 0
    For i = 1 To noOfAttribute
        attributeStartPosition = InStr(attributeEndPosition + 1, strLine, Chr$(34)) + 1
        If attributeStartPosition < attributeEndPosition Then
            readAttribute = ""
            Exit Function
        End If
        attributeEndPosition = InStr(attributeStartPosition, strLine, Chr$(34))
    Next i
    If attributeEndPosition <> 0 And attributeStartPosition > 1 Then
'FIXIT: Replace 'Mid' function with 'Mid$' function                                        FixIT90210ae-R9757-R1B8ZE
        readAttribute = Mid(strLine, attributeStartPosition, attributeEndPosition - attributeStartPosition)
    Else
        readAttribute = ""
    End If

End Function

Private Function readLineHeader(ByVal strLine As String) As String
    Dim sLine As String
    
'FIXIT: Replace 'Left' function with 'Left$' function                                      FixIT90210ae-R9757-R1B8ZE
    sLine = Left(strLine, InStr(1, strLine, ">"))
    
    If InStr(1, sLine, Chr$(32)) <> 0 Then
'FIXIT: Replace 'Mid' function with 'Mid$' function                                        FixIT90210ae-R9757-R1B8ZE
'FIXIT: Replace 'Left' function with 'Left$' function                                      FixIT90210ae-R9757-R1B8ZE
        readLineHeader = Mid(Left(sLine, InStr(1, sLine, Chr$(32)) - 1), 2)
    Else
'FIXIT: Replace 'Mid' function with 'Mid$' function                                        FixIT90210ae-R9757-R1B8ZE
        readLineHeader = Mid(sLine, 2, Len(sLine) - 2)
    End If
End Function

Private Function readValue(strLine As String) As String
    Dim startPosition As Long
    Dim endPosition As Long

    startPosition = InStr(1, strLine, ">") + 1
    endPosition = InStr(startPosition, strLine, "<")
    
    If endPosition <> 0 Then
'FIXIT: Replace 'Mid' function with 'Mid$' function                                        FixIT90210ae-R9757-R1B8ZE
        readValue = Mid(strLine, startPosition, endPosition - startPosition)
    Else
        readValue = ""
    End If
    
End Function

Private Function cleanLine(strLine As String) As String
    Dim char As String
    Dim i As Integer
    
    For i = 1 To Len(strLine)
'FIXIT: Replace 'Mid' function with 'Mid$' function                                        FixIT90210ae-R9757-R1B8ZE
        char = Mid(strLine, i, 1)
        If Asc(char) = 9 Then char = "  "
        cleanLine = cleanLine & char
    Next i
'FIXIT: Replace 'Trim' function with 'Trim$' function                                      FixIT90210ae-R9757-R1B8ZE
    cleanLine = Trim(cleanLine)
End Function

