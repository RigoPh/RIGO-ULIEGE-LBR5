VERSION 5.00
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmGeometricProperties 
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Geometric Properties"
   ClientHeight    =   3435
   ClientLeft      =   8655
   ClientTop       =   3810
   ClientWidth     =   4485
   Icon            =   "frmWeight_GravityCenter.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3435
   ScaleWidth      =   4485
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox txtCrossSectionNet 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1440
      Locked          =   -1  'True
      TabIndex        =   24
      Text            =   "Text1"
      Top             =   480
      Width           =   975
   End
   Begin VB.TextBox txtCrossSectionGross 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3000
      Locked          =   -1  'True
      TabIndex        =   23
      Text            =   "Text1"
      Top             =   480
      Width           =   975
   End
   Begin VB.TextBox txtSectionModulusNet 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1440
      Locked          =   -1  'True
      TabIndex        =   19
      Text            =   "Text1"
      Top             =   2400
      Width           =   975
   End
   Begin VB.TextBox txtSectionModulusGross 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3000
      Locked          =   -1  'True
      TabIndex        =   18
      Text            =   "Text1"
      Top             =   2400
      Width           =   975
   End
   Begin VB.TextBox txtInertiaYYGross 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3000
      Locked          =   -1  'True
      TabIndex        =   14
      Text            =   "Text1"
      Top             =   1920
      Width           =   975
   End
   Begin VB.TextBox txtInertiaYYNet 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1440
      Locked          =   -1  'True
      TabIndex        =   13
      Text            =   "Text1"
      Top             =   1920
      Width           =   975
   End
   Begin VB.TextBox txtNeutralAxisNet 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1440
      Locked          =   -1  'True
      TabIndex        =   9
      Text            =   "Text1"
      Top             =   1440
      Width           =   975
   End
   Begin VB.TextBox txtNeutralAxisGross 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3000
      Locked          =   -1  'True
      TabIndex        =   8
      Text            =   "Text1"
      Top             =   1440
      Width           =   975
   End
   Begin VB.TextBox txtGravityCenterGross 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   3000
      Locked          =   -1  'True
      TabIndex        =   6
      Text            =   "Text1"
      Top             =   960
      Width           =   975
   End
   Begin VB.TextBox txtGravityCenterNet 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1440
      Locked          =   -1  'True
      TabIndex        =   4
      Text            =   "Text1"
      Top             =   960
      Width           =   975
   End
   Begin VB.Label Label17 
      AutoSize        =   -1  'True
      Caption         =   "Cross Section:"
      Height          =   195
      Left            =   120
      TabIndex        =   27
      Top             =   480
      Width           =   1020
   End
   Begin VB.Label Label16 
      AutoSize        =   -1  'True
      Caption         =   "[m2]"
      Height          =   195
      Left            =   2520
      TabIndex        =   26
      Top             =   480
      Width           =   300
   End
   Begin VB.Label Label15 
      AutoSize        =   -1  'True
      Caption         =   "[m2]"
      Height          =   195
      Left            =   4080
      TabIndex        =   25
      Top             =   480
      Width           =   300
   End
   Begin VB.Label Label14 
      AutoSize        =   -1  'True
      Caption         =   "Section Modulus:"
      Height          =   195
      Left            =   120
      TabIndex        =   22
      Top             =   2400
      Width           =   1230
   End
   Begin VB.Label Label13 
      AutoSize        =   -1  'True
      Caption         =   "[m3]"
      Height          =   195
      Left            =   2520
      TabIndex        =   21
      Top             =   2400
      Width           =   300
   End
   Begin VB.Label Label12 
      AutoSize        =   -1  'True
      Caption         =   "[m3]"
      Height          =   195
      Left            =   4080
      TabIndex        =   20
      Top             =   2400
      Width           =   300
   End
   Begin VB.Label Label11 
      AutoSize        =   -1  'True
      Caption         =   "[m4]"
      Height          =   195
      Left            =   4080
      TabIndex        =   17
      Top             =   1920
      Width           =   300
   End
   Begin VB.Label Label10 
      AutoSize        =   -1  'True
      Caption         =   "[m4]"
      Height          =   195
      Left            =   2520
      TabIndex        =   16
      Top             =   1920
      Width           =   300
   End
   Begin VB.Label Label9 
      AutoSize        =   -1  'True
      Caption         =   "Inertia YY:"
      Height          =   195
      Left            =   120
      TabIndex        =   15
      Top             =   1920
      Width           =   735
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Default         =   -1  'True
      Height          =   375
      Left            =   3240
      TabIndex        =   0
      Top             =   2880
      Width           =   1095
      Caption         =   "Close"
      PicturePosition =   327683
      Size            =   "1931;661"
      Accelerator     =   67
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin VB.Label Label8 
      AutoSize        =   -1  'True
      Caption         =   "Neutral Axis:"
      Height          =   195
      Left            =   120
      TabIndex        =   12
      Top             =   1440
      Width           =   885
   End
   Begin VB.Label Label7 
      AutoSize        =   -1  'True
      Caption         =   "[m]"
      Height          =   195
      Left            =   2520
      TabIndex        =   11
      Top             =   1440
      Width           =   210
   End
   Begin VB.Label Label6 
      AutoSize        =   -1  'True
      Caption         =   "[m]"
      Height          =   195
      Left            =   4080
      TabIndex        =   10
      Top             =   1440
      Width           =   210
   End
   Begin VB.Label Label5 
      AutoSize        =   -1  'True
      Caption         =   "[m]"
      Height          =   195
      Left            =   4080
      TabIndex        =   7
      Top             =   960
      Width           =   210
   End
   Begin VB.Label Label4 
      AutoSize        =   -1  'True
      Caption         =   "[m]"
      Height          =   195
      Left            =   2520
      TabIndex        =   5
      Top             =   960
      Width           =   210
   End
   Begin VB.Label Label3 
      AutoSize        =   -1  'True
      Caption         =   "Gross"
      Height          =   195
      Left            =   3000
      TabIndex        =   3
      Top             =   120
      Width           =   405
   End
   Begin VB.Label Label2 
      AutoSize        =   -1  'True
      Caption         =   "Net"
      Height          =   195
      Left            =   1440
      TabIndex        =   2
      Top             =   120
      Width           =   255
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Gravity Center:"
      Height          =   195
      Left            =   120
      TabIndex        =   1
      Top             =   960
      Width           =   1050
   End
End
Attribute VB_Name = "frmGeometricProperties"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer

Public Function getGravityCenterNet() As Double
    On Error GoTo getGravityCenterNetErr
    'net scantlings, equivalent plate (plate + stiffeners + frames (2 sets) + girders)
    ProjectIndex = ActiveProject
    Dim oPanel As cPanel, oGirder As cGirder
    Dim sectst1 As Double, sectst2 As Double, sectfr1 As Double, sectfr2 As Double, sectgr As Double
    Dim spst1 As Double, spst2 As Double, spfr1 As Double, spfr2 As Double, larg As Double
    Dim nrst1 As Double, nrst2 As Double, nrfr1 As Double, nrfr2 As Double
    Dim plth As Double, eqplth As Double
    Dim icorrth As Integer 'corrosion thickness (zero if not considered)
    Dim AiDENSi As Double, ZiAiDENSi As Double
    Dim Zi As Double, AI As Double, AiZi As Double, SumAi As Double
    Dim z1 As Double, z2 As Double, Zg As Double, ZZg As Double
    Zi = 0
    AI = 0
    AiZi = 0
    AiDENSi = 0
    ZiAiDENSi = 0
    
    For Each oPanel In Project.Item(ProjectIndex).colPanel
        larg = oPanel.cGeometry.PanelWidth
        Select Case oPanel.pType
            Case Plate
                plth = oPanel.cScantlings.NetThickness
                With oPanel.cScantlings.cPrimaryStiffeners
                    sectst1 = .WebHeight * (.WebThickness + 0) + .FlangeWidth * (.FlangeThickness + 0)
                    spst1 = .Spacing
                    nrst1 = .NumberOfStiffeners
                End With
                With oPanel.cScantlings.cSecondaryStiffeners
                    sectst2 = .WebHeight * .WebThickness + .FlangeWidth * .FlangeThickness
                    spst2 = .Spacing
                End With
                With oPanel.cScantlings.cPrimaryFrames
                    sectfr1 = .WebHeight * (.WebThickness + 0) + .FlangeWidth * (.FlangeThickness + 0)
                    spfr1 = .Spacing
                End With
                With oPanel.cScantlings.cSecondaryFrames
                    sectfr2 = .WebHeight * .WebThickness + .FlangeWidth * .FlangeThickness
                    spfr2 = .Spacing
                End With
                
                'equivalent thickness (girders excluded)
                eqplth = plth + Divide(sectst1 * nrst1, larg) + Divide(sectst2, spst2) + Divide(sectfr1, spfr1) + Divide(sectfr2, spfr2)
            Case Beam
                plth = 0
                eqplth = 0
                With oPanel.cScantlings.cPrimaryFrames
                    Select Case oPanel.cScantlings.BeamSection
                        Case bsCircle
                            sectfr1 = .WebHeight ^ 2 / 4 * PI - (.WebHeight - 2 * (oPanel.cScantlings.NetThickness + 0)) ^ 2 / 4 * PI
                        Case bsSquare
                            sectfr1 = 2 * .WebHeight - 2 * (.WebHeight - 2 * (oPanel.cScantlings.NetThickness + 0))
                        Case bsDoubleT
                            sectfr1 = .WebHeight * (.WebThickness + 0) + 2 * .FlangeWidth * (.FlangeThickness + 0)
                    End Select
                    spfr1 = .Spacing
                End With
                eqplth = Divide(sectfr1, spfr1)
        End Select
        
        'equivalent thickness (girders included)
        For Each oGirder In oPanel.cScantlings.colGirder
            With oGirder
                sectgr = .WebHeight * .WebThickness + .FlangeWidth * .FlangeThickness
            End With
            eqplth = eqplth + Divide(sectgr, larg)
        Next oGirder
        
        'panel equivalent section
        AI = eqplth * larg
        SumAi = SumAi + AI
        AiDENSi = AiDENSi + AI * oPanel.cMaterial.SpecificWeight
        
        'panel gravity center
        With Project.Item(ProjectIndex).colNodes
            z1 = -.Item(oPanel.cGeometry.InNode).z
            z2 = -.Item(oPanel.cGeometry.OutNode).z
            Zi = (z1 + z2) / 2
        End With
        
        'AiZi
        AiZi = AiZi + AI * Zi
        ZiAiDENSi = ZiAiDENSi + AI * Zi * oPanel.cMaterial.SpecificWeight
    Next oPanel
    
    ZZg = Divide(AiZi, SumAi)
    ZZg = Divide(ZiAiDENSi, AiDENSi)
    
    getGravityCenterNet = Round(ZZg, 5)
    
    Exit Function
getGravityCenterNetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modFunctions: Function getGravityCenterNet")
End Function

Public Function getGravityCenterGross() As Double
    On Error GoTo getGravityCenterGrossErr
    'net scantlings, equivalent plate (plate + stiffeners + frames (2 sets) + girders)
    ProjectIndex = ActiveProject
    Dim oPanel As cPanel, oGirder As cGirder
    Dim sectst1 As Double, sectst2 As Double, sectfr1 As Double, sectfr2 As Double, sectgr As Double
    Dim spst1 As Double, spst2 As Double, spfr1 As Double, spfr2 As Double, larg As Double
    Dim nrst1 As Double, nrst2 As Double, nrfr1 As Double, nrfr2 As Double
    Dim plth As Double, eqplth As Double
    Dim icorrth As Integer 'corrosion thickness (zero if not considered)
    Dim AiDENSi As Double, ZiAiDENSi As Double
    Dim Zi As Double, AI As Double, AiZi As Double, SumAi As Double
    Dim z1 As Double, z2 As Double, Zg As Double, ZZg As Double
    Zi = 0
    AI = 0
    AiZi = 0
    AiDENSi = 0
    ZiAiDENSi = 0
    
    For Each oPanel In Project.Item(ProjectIndex).colPanel
        larg = oPanel.cGeometry.PanelWidth
        Select Case oPanel.pType
            Case Plate
                plth = oPanel.cScantlings.GrossThickness
                With oPanel.cScantlings.cPrimaryStiffeners
                    sectst1 = .WebHeight * (.WebThickness + .CorrosionThickness) + .FlangeWidth * (.FlangeThickness + .CorrosionThickness)
                    spst1 = .Spacing
                    nrst1 = .NumberOfStiffeners
                End With
                With oPanel.cScantlings.cSecondaryStiffeners
                    sectst2 = .WebHeight * .WebThickness + .FlangeWidth * .FlangeThickness
                    spst2 = .Spacing
                End With
                With oPanel.cScantlings.cPrimaryFrames
                    sectfr1 = .WebHeight * (.WebThickness + .CorrosionThickness) + .FlangeWidth * (.FlangeThickness + .CorrosionThickness)
                    spfr1 = .Spacing
                End With
                With oPanel.cScantlings.cSecondaryFrames
                    sectfr2 = .WebHeight * .WebThickness + .FlangeWidth * .FlangeThickness
                    spfr2 = .Spacing
                End With
                
                'equivalent thickness (girders excluded)
                eqplth = plth + Divide(sectst1 * nrst1, larg) + Divide(sectst2, spst2) + Divide(sectfr1, spfr1) + Divide(sectfr2, spfr2)
            Case Beam
                plth = 0
                eqplth = 0
                With oPanel.cScantlings.cPrimaryFrames
                    Select Case oPanel.cScantlings.BeamSection
                        Case bsCircle
                            sectfr1 = .WebHeight ^ 2 / 4 * PI - (.WebHeight - 2 * (oPanel.cScantlings.NetThickness + .CorrosionThickness)) ^ 2 / 4 * PI
                        Case bsSquare
                            sectfr1 = 2 * .WebHeight - 2 * (.WebHeight - 2 * (oPanel.cScantlings.NetThickness + .CorrosionThickness))
                        Case bsDoubleT
                            sectfr1 = .WebHeight * (.WebThickness + .CorrosionThickness) + 2 * .FlangeWidth * (.FlangeThickness + .CorrosionThickness)
                    End Select
                    spfr1 = .Spacing
                End With
                eqplth = Divide(sectfr1, spfr1)
        End Select
        
        'equivalent thickness (girders included)
        For Each oGirder In oPanel.cScantlings.colGirder
            With oGirder
                sectgr = .WebHeight * .WebThickness + .FlangeWidth * .FlangeThickness
            End With
            eqplth = eqplth + Divide(sectgr, larg)
        Next oGirder
        
        'panel equivalent section
        AI = eqplth * larg
        SumAi = SumAi + AI
        AiDENSi = AiDENSi + AI * oPanel.cMaterial.SpecificWeight
        
        'panel gravity center
        With Project.Item(ProjectIndex).colNodes
            z1 = -.Item(oPanel.cGeometry.InNode).z
            z2 = -.Item(oPanel.cGeometry.OutNode).z
            Zi = (z1 + z2) / 2
        End With
        
        'AiZi
        AiZi = AiZi + AI * Zi
        ZiAiDENSi = ZiAiDENSi + AI * Zi * oPanel.cMaterial.SpecificWeight
    Next oPanel
    
    ZZg = Divide(AiZi, SumAi)
    ZZg = Divide(ZiAiDENSi, AiDENSi)
    
    getGravityCenterGross = Round(ZZg, 5)
    
    Exit Function
getGravityCenterGrossErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modFunctions: Function getGravityCenterGross")
End Function

Public Function getNeutralAxisNet() As Double
    On Error GoTo getNeutralAxisNetErr
    'net scantlings, equivalent plate (plate + stiffeners + frames (2 sets) + girders)
    ProjectIndex = ActiveProject
    Dim oPanel As cPanel, oGirder As cGirder
    Dim sectst1 As Double, sectst2 As Double, sectfr1 As Double, sectfr2 As Double, sectgr As Double
    Dim spst1 As Double, spst2 As Double, spfr1 As Double, spfr2 As Double, larg As Double
    Dim nrst1 As Double, nrst2 As Double, nrfr1 As Double, nrfr2 As Double
    Dim plth As Double, eqplth As Double
    Dim icorrth As Integer 'corrosion thickness (zero if not considered)
    Dim AiDENSi As Double, ZiAiDENSi As Double
    Dim Zi As Double, AI As Double, AiZi As Double, SumAi As Double
    Dim z1 As Double, z2 As Double, Zg As Double, ZZg As Double
    Zi = 0
    AI = 0
    AiZi = 0
    AiDENSi = 0
    ZiAiDENSi = 0
    
    For Each oPanel In Project.Item(ProjectIndex).colPanel
        larg = oPanel.cGeometry.PanelWidth
        Select Case oPanel.pType
            Case Plate
                plth = oPanel.cScantlings.NetThickness
                With oPanel.cScantlings.cPrimaryStiffeners
                    sectst1 = .WebHeight * (.WebThickness + 0) + .FlangeWidth * (.FlangeThickness + 0)
                    spst1 = .Spacing
'                    nrst1 = IIf(oPanel.cScantlings.cPrimaryStiffeners.DistributionMode = "EE1", _
'                            oPanel.cGeometry.PanelWidth / oPanel.cScantlings.cPrimaryStiffeners.Spacing - 1, _
'                            oPanel.cGeometry.PanelWidth / oPanel.cScantlings.cPrimaryStiffeners.Spacing) '.NumberOfStiffeners
                    nrst1 = .NumberOfStiffeners
                End With
                With oPanel.cScantlings.cSecondaryStiffeners
                    sectst2 = .WebHeight * .WebThickness + .FlangeWidth * .FlangeThickness
                    spst2 = .Spacing
                End With
                                
                'equivalent thickness (girders excluded)
                eqplth = plth + Divide(sectst1 * nrst1, larg) + Divide(sectst2, spst2)
            Case Beam
                plth = 0
                eqplth = 0
                GoTo nextpan
        End Select
        
        'equivalent thickness (girders included)
        For Each oGirder In oPanel.cScantlings.colGirder
            With oGirder
                sectgr = .WebHeight * .WebThickness + .FlangeWidth * .FlangeThickness
            End With
            eqplth = eqplth + Divide(sectgr, larg)
        Next oGirder
        
        'participation
        eqplth = eqplth * oPanel.cGeometry.Participation
        
        'panel equivalent section
        AI = eqplth * larg
        SumAi = SumAi + AI
        AiDENSi = AiDENSi + AI * oPanel.cMaterial.SpecificWeight
        
        'panel gravity center
        With Project.Item(ProjectIndex).colNodes
            z1 = -.Item(oPanel.cGeometry.InNode).z
            z2 = -.Item(oPanel.cGeometry.OutNode).z
            Zi = (z1 + z2) / 2
        End With
        
        'AiZi
         AiZi = AiZi + AI * Zi
        ZiAiDENSi = ZiAiDENSi + AI * Zi * oPanel.cMaterial.SpecificWeight
nextpan:
    Next oPanel
    
    ZZg = Divide(AiZi, SumAi)
    ZZg = Divide(ZiAiDENSi, AiDENSi)
    
    getNeutralAxisNet = Round(ZZg, 5)
    Exit Function
getNeutralAxisNetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modFunctions: Function getNeutralAxisNet")
End Function

Public Function getNeutralAxisGross() As Double
    On Error GoTo getNeutralAxisGrossErr
    'net scantlings, equivalent plate (plate + stiffeners + frames (2 sets) + girders)
    ProjectIndex = ActiveProject
    Dim oPanel As cPanel, oGirder As cGirder
    Dim sectst1 As Double, sectst2 As Double, sectfr1 As Double, sectfr2 As Double, sectgr As Double
    Dim spst1 As Double, spst2 As Double, spfr1 As Double, spfr2 As Double, larg As Double
    Dim nrst1 As Double, nrst2 As Double, nrfr1 As Double, nrfr2 As Double
    Dim plth As Double, eqplth As Double
    Dim icorrth As Integer 'corrosion thickness (zero if not considered)
    Dim AiDENSi As Double, ZiAiDENSi As Double
    Dim Zi As Double, AI As Double, AiZi As Double, SumAi As Double
    Dim z1 As Double, z2 As Double, Zg As Double, ZZg As Double
    Zi = 0
    AI = 0
    AiZi = 0
    AiDENSi = 0
    ZiAiDENSi = 0
    
    For Each oPanel In Project.Item(ProjectIndex).colPanel
        larg = oPanel.cGeometry.PanelWidth
        Select Case oPanel.pType
            Case Plate
                plth = oPanel.cScantlings.GrossThickness
                With oPanel.cScantlings.cPrimaryStiffeners
                    sectst1 = .WebHeight * (.WebThickness + .CorrosionThickness) + .FlangeWidth * (.FlangeThickness + .CorrosionThickness)
                    spst1 = .Spacing
                    nrst1 = .NumberOfStiffeners
                End With
                With oPanel.cScantlings.cSecondaryStiffeners
                    sectst2 = .WebHeight * .WebThickness + .FlangeWidth * .FlangeThickness
                    spst2 = .Spacing
                End With
                                
                'equivalent thickness (girders excluded)
                eqplth = plth + Divide(sectst1 * nrst1, larg) + Divide(sectst2, spst2)
            Case Beam
                plth = 0
                eqplth = 0
                GoTo nextpan
        End Select
        
        'equivalent thickness (girders included)
        For Each oGirder In oPanel.cScantlings.colGirder
            With oGirder
                sectgr = .WebHeight * (.WebThickness + oPanel.cScantlings.cPrimaryStiffeners.CorrosionThickness) + .FlangeWidth * (.FlangeThickness + oPanel.cScantlings.cPrimaryStiffeners.CorrosionThickness)
                'sectgr = .WebHeight * (.WebThickness + 0) + .FlangeWidth * (.FlangeThickness + 0)
            End With
            eqplth = eqplth + Divide(sectgr, larg)
        Next oGirder
        
        'participation
        eqplth = eqplth * oPanel.cGeometry.Participation
        
        'panel equivalent section
        AI = eqplth * larg
        SumAi = SumAi + AI
        AiDENSi = AiDENSi + AI * oPanel.cMaterial.SpecificWeight
        
        'panel gravity center
        With Project.Item(ProjectIndex).colNodes
            z1 = -.Item(oPanel.cGeometry.InNode).z
            z2 = -.Item(oPanel.cGeometry.OutNode).z
            Zi = (z1 + z2) / 2
        End With
        
        'AiZi
         AiZi = AiZi + AI * Zi
        ZiAiDENSi = ZiAiDENSi + AI * Zi * oPanel.cMaterial.SpecificWeight
nextpan:
    Next oPanel
    
    ZZg = Divide(AiZi, SumAi)
    ZZg = Divide(ZiAiDENSi, AiDENSi)
    
    getNeutralAxisGross = Round(ZZg, 5)
    
    Exit Function
getNeutralAxisGrossErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modFunctions: Function getNeutralAxisGross")
End Function

Public Function getInertiaYYNet() As Double
    On Error GoTo getInertiaYYNetErr
    'net scantlings, equivalent plate (plate + stiffeners + frames (2 sets) + girders)
    ProjectIndex = ActiveProject
    Dim oPanel As cPanel, oGirder As cGirder
    Dim sectst1 As Double, sectst2 As Double, sectfr1 As Double, sectfr2 As Double, sectgr As Double
    Dim spst1 As Double, spst2 As Double, spfr1 As Double, spfr2 As Double, larg As Double
    Dim nrst1 As Double, nrst2 As Double, nrfr1 As Double, nrfr2 As Double
    Dim plth As Double, eqplth As Double
    Dim icorrth As Integer 'corrosion thickness (zero if not considered)
    Dim AiDENSi As Double, ZiAiDENSi As Double
    Dim Zi As Double, AI As Double, AiZi As Double, SumAi As Double
    Dim z1 As Double, z2 As Double, Zg As Double, ZZg As Double
    Dim alfa As Double 'panel angle
    Dim dx As Double
    Dim Iyy As Double
    Dim zz As Double
    
    
    Dim Ix As Double, Iy As Double, Ixy As Double, alfa2 As Double
    
    Zi = 0
    AI = 0
    AiZi = 0
    AiDENSi = 0
    ZiAiDENSi = 0
    ZZg = Val(txtNeutralAxisNet)
    
    For Each oPanel In Project.Item(ProjectIndex).colPanel
        larg = oPanel.cGeometry.PanelWidth
        Select Case oPanel.pType
            Case Plate
                plth = oPanel.cScantlings.NetThickness
                With oPanel.cScantlings.cPrimaryStiffeners
                    sectst1 = .WebHeight * (.WebThickness + 0) + .FlangeWidth * (.FlangeThickness + 0)
                    spst1 = .Spacing
                    nrst1 = .NumberOfStiffeners
                End With
                With oPanel.cScantlings.cSecondaryStiffeners
                    sectst2 = .WebHeight * .WebThickness + .FlangeWidth * .FlangeThickness
                    spst2 = .Spacing
                End With
                                
                'equivalent thickness (girders excluded)
                eqplth = plth + Divide(sectst1 * nrst1, larg) + Divide(sectst2, spst2)
            Case Beam
                plth = 0
                eqplth = 0
                GoTo nextpan
        End Select
        
        'equivalent thickness (girders included)
        For Each oGirder In oPanel.cScantlings.colGirder
            With oGirder
                sectgr = .WebHeight * .WebThickness + .FlangeWidth * .FlangeThickness
            End With
            eqplth = eqplth + Divide(sectgr, larg)
        Next oGirder
        
        'participation
        eqplth = eqplth * oPanel.cGeometry.Participation
        
        'panel equivalent section
        AI = eqplth * larg
        
        'panel gravity center
        With Project.Item(ProjectIndex).colNodes
            z1 = -.Item(oPanel.cGeometry.InNode).z
            z2 = -.Item(oPanel.cGeometry.OutNode).z
            Zi = (z1 + z2) / 2
        End With
               
        'Inertia
        alfa = oPanel.cGeometry.PanelAngle
        dx = Abs(Divide(eqplth, Sin(alfa * PI / 180)))
        If dx = 0 Or dx > larg Then
            dx = larg
        End If
        
        zz = Abs((z2 - z1) / 2)
        If zz < eqplth / 2 Then zz = eqplth / 2
        
'        Iyy = Iyy + 2 * dx * zz ^ 3 / 3 + (Abs(ZZg - Zi)) ^ 2 * AI
        Ix = larg * eqplth ^ 3 / 12
        Iy = larg ^ 3 * eqplth / 12
        Ixy = larg ^ 2 * eqplth ^ 2 / 12
        alfa2 = alfa * 2
        Iyy = Iyy + (Ix + Iy) / 2 + ((Ix - Iy) / 2) * Cos(alfa2 * PI / 180) - Ixy * Sin(alfa2 * PI / 180) + AI * (ZZg - Zi) ^ 2
        
        
nextpan:
    Next oPanel
    
    getInertiaYYNet = Round(Iyy * 2, 5)
    
    Exit Function
getInertiaYYNetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modFunctions: Function getInertiaYYNet")
End Function

Public Function getInertiaYYGross() As Double
    On Error GoTo getInertiaYYGrossErr
    'net scantlings, equivalent plate (plate + stiffeners + frames (2 sets) + girders)
    ProjectIndex = ActiveProject
    Dim oPanel As cPanel, oGirder As cGirder
    Dim sectst1 As Double, sectst2 As Double, sectfr1 As Double, sectfr2 As Double, sectgr As Double
    Dim spst1 As Double, spst2 As Double, spfr1 As Double, spfr2 As Double, larg As Double
    Dim nrst1 As Double, nrst2 As Double, nrfr1 As Double, nrfr2 As Double
    Dim plth As Double, eqplth As Double
    Dim icorrth As Integer 'corrosion thickness (zero if not considered)
    Dim AiDENSi As Double, ZiAiDENSi As Double
    Dim Zi As Double, AI As Double, AiZi As Double, SumAi As Double
    Dim z1 As Double, z2 As Double, Zg As Double, ZZg As Double
    Dim alfa As Double 'panel angle
    Dim dx As Double
    Dim Iyy As Double
    Dim zz As Double
        
    Dim Ix As Double, Iy As Double, Ixy As Double, alfa2 As Double

    Zi = 0
    AI = 0
    AiZi = 0
    AiDENSi = 0
    ZiAiDENSi = 0
    ZZg = Val(txtNeutralAxisGross)
    
    For Each oPanel In Project.Item(ProjectIndex).colPanel
        larg = oPanel.cGeometry.PanelWidth
        Select Case oPanel.pType
            Case Plate
                plth = oPanel.cScantlings.GrossThickness
                With oPanel.cScantlings.cPrimaryStiffeners
                    sectst1 = .WebHeight * (.WebThickness + .CorrosionThickness) + .FlangeWidth * (.FlangeThickness + .CorrosionThickness)
                    spst1 = .Spacing
                    nrst1 = .NumberOfStiffeners
                End With
                With oPanel.cScantlings.cSecondaryStiffeners
                    sectst2 = .WebHeight * .WebThickness + .FlangeWidth * .FlangeThickness
                    spst2 = .Spacing
                End With
                                
                'equivalent thickness (girders excluded)
                eqplth = plth + Divide(sectst1 * nrst1, larg) + Divide(sectst2, spst2)
            Case Beam
                plth = 0
                eqplth = 0
                GoTo nextpan
        End Select
        
        'equivalent thickness (girders included)
        For Each oGirder In oPanel.cScantlings.colGirder
            With oGirder
                sectgr = .WebHeight * (.WebThickness + oPanel.cScantlings.cPrimaryStiffeners.CorrosionThickness) + .FlangeWidth * (.FlangeThickness + oPanel.cScantlings.cPrimaryStiffeners.CorrosionThickness)
            End With
            eqplth = eqplth + Divide(sectgr, larg)
        Next oGirder
        
        'participation
        eqplth = eqplth * oPanel.cGeometry.Participation
        
        'panel equivalent section
        AI = eqplth * larg
        
        'panel gravity center
        With Project.Item(ProjectIndex).colNodes
            z1 = -.Item(oPanel.cGeometry.InNode).z
            z2 = -.Item(oPanel.cGeometry.OutNode).z
            Zi = (z1 + z2) / 2
        End With
               
        'Inertia
        alfa = oPanel.cGeometry.PanelAngle
        dx = Abs(Divide(eqplth, Sin(alfa * PI / 180)))
        If dx = 0 Or dx > larg Then
            dx = larg
        End If
        
        zz = Abs((z2 - z1) / 2)
        If zz < eqplth / 2 Then zz = eqplth / 2
        
        'Iyy = Iyy + 2 * dx * zz ^ 3 / 3 + (Abs(ZZg - Zi)) ^ 2 * AI
        Ix = larg * eqplth ^ 3 / 12
        Iy = larg ^ 3 * eqplth / 12
        Ixy = larg ^ 2 * eqplth ^ 2 / 12
        alfa2 = alfa * 2
        Iyy = Iyy + (Ix + Iy) / 2 + ((Ix - Iy) / 2) * Cos(alfa2 * PI / 180) - Ixy * Sin(alfa2 * PI / 180) + AI * (ZZg - Zi) ^ 2

        
nextpan:
    Next oPanel
    
    getInertiaYYGross = Round(Iyy * 2, 5)
    
    Exit Function
getInertiaYYGrossErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modFunctions: Function getInertiaYYGross")
End Function

Public Function GetSectionModulusNet() As Double
    On Error GoTo GetSectionModulusNetErr
    ProjectIndex = ActiveProject
    Dim ZMin As Double, ZMax As Double, Iyy As Double, NA As Double
    Dim Height As Double
    ZMax = -Project.Item(ProjectIndex).ZMin
    ZMin = -Project.Item(ProjectIndex).ZMax
    Height = ZMax - ZMin
    NA = Val(txtNeutralAxisNet)
    Iyy = Val(txtInertiaYYNet)
    
    If NA > Height / 2 Then
        GetSectionModulusNet = Round(Iyy / NA, 5)
    Else
        GetSectionModulusNet = Round(Iyy / (Height - NA), 5)
    End If
    
    Exit Function
GetSectionModulusNetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modFunctions: Function GetSectionModulusNet")
End Function

Public Function GetSectionModulusGross() As Double
    On Error GoTo GetSectionModulusGrossErr
    ProjectIndex = ActiveProject
    Dim ZMin As Double, ZMax As Double, Iyy As Double, NA As Double
    Dim Height As Double
    ZMax = -Project.Item(ProjectIndex).ZMin
    ZMin = -Project.Item(ProjectIndex).ZMax
    Height = ZMax - ZMin
    NA = Val(txtNeutralAxisGross)
    Iyy = Val(txtInertiaYYGross)
    
    If NA > Height / 2 Then
        GetSectionModulusGross = Round(Iyy / NA, 5)
    Else
        GetSectionModulusGross = Round(Iyy / (Height - NA), 5)
    End If
    
    Exit Function
GetSectionModulusGrossErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modFunctions: Function GetSectionModulusGross")
End Function

Public Function GetCrossSectionNet() As Double
    On Error GoTo GetCrossSectionNetErr
    ProjectIndex = ActiveProject
    Dim oPanel As cPanel, oGirder As cGirder
    Dim sectst1 As Double, sectst2 As Double, sectfr1 As Double, sectfr2 As Double, sectgr As Double
    Dim spst1 As Double, spst2 As Double, spfr1 As Double, spfr2 As Double, larg As Double
    Dim nrst1 As Double, nrst2 As Double, nrfr1 As Double, nrfr2 As Double
    Dim plth As Double, eqplth As Double
    Dim icorrth As Integer 'corrosion thickness (zero if not considered)
    Dim AiDENSi As Double, ZiAiDENSi As Double
    Dim AI As Double, SumAi As Double
    Dim z1 As Double, z2 As Double, Zg As Double, ZZg As Double
    
    For Each oPanel In Project.Item(ProjectIndex).colPanel
        larg = oPanel.cGeometry.PanelWidth
        Select Case oPanel.pType
            Case Plate
                plth = oPanel.cScantlings.NetThickness
                With oPanel.cScantlings.cPrimaryStiffeners
                    sectst1 = .WebHeight * (.WebThickness + 0) + .FlangeWidth * (.FlangeThickness + 0)
                    spst1 = .Spacing
                    nrst1 = .NumberOfStiffeners
                End With
                With oPanel.cScantlings.cSecondaryStiffeners
                    sectst2 = .WebHeight * .WebThickness + .FlangeWidth * .FlangeThickness
                    spst2 = .Spacing
                End With
                                
                'equivalent thickness (girders excluded)
                eqplth = plth + Divide(sectst1 * nrst1, larg) + Divide(sectst2, spst2)
            Case Beam
                plth = 0
                eqplth = 0
                GoTo nextpan
        End Select
        
        'equivalent thickness (girders included)
        For Each oGirder In oPanel.cScantlings.colGirder
            With oGirder
                sectgr = .WebHeight * .WebThickness + .FlangeWidth * .FlangeThickness
            End With
            eqplth = eqplth + Divide(sectgr, larg)
        Next oGirder
        
        'participation
        'eqplth = eqplth * oPanel.cGeometry.Participation
        
        'panel equivalent section
        AI = eqplth * larg
        SumAi = SumAi + AI
nextpan:
    Next oPanel
    
    GetCrossSectionNet = Round(SumAi, 5)
    
    Exit Function
GetCrossSectionNetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modFunctions: Function GetCrossSectionNet")
End Function

Public Function GetCrossSectionGross() As Double
    On Error GoTo GetCrossSectionGrossErr
    ProjectIndex = ActiveProject
    Dim oPanel As cPanel, oGirder As cGirder
    Dim sectst1 As Double, sectst2 As Double, sectfr1 As Double, sectfr2 As Double, sectgr As Double
    Dim spst1 As Double, spst2 As Double, spfr1 As Double, spfr2 As Double, larg As Double
    Dim nrst1 As Double, nrst2 As Double, nrfr1 As Double, nrfr2 As Double
    Dim plth As Double, eqplth As Double
    Dim icorrth As Integer 'corrosion thickness (zero if not considered)
    Dim AiDENSi As Double, ZiAiDENSi As Double
    Dim AI As Double, SumAi As Double
    Dim z1 As Double, z2 As Double, Zg As Double, ZZg As Double
    
    For Each oPanel In Project.Item(ProjectIndex).colPanel
        larg = oPanel.cGeometry.PanelWidth
        Select Case oPanel.pType
            Case Plate
                plth = oPanel.cScantlings.GrossThickness
                With oPanel.cScantlings.cPrimaryStiffeners
                    sectst1 = .WebHeight * (.WebThickness + .CorrosionThickness) + .FlangeWidth * (.FlangeThickness + .CorrosionThickness)
                    spst1 = .Spacing
                    nrst1 = .NumberOfStiffeners
                End With
                With oPanel.cScantlings.cSecondaryStiffeners
                    sectst2 = .WebHeight * .WebThickness + .FlangeWidth * .FlangeThickness
                    spst2 = .Spacing
                End With
                                
                'equivalent thickness (girders excluded)
                eqplth = plth + Divide(sectst1 * nrst1, larg) + Divide(sectst2, spst2)
            Case Beam
                plth = 0
                eqplth = 0
                GoTo nextpan
        End Select
        
        'equivalent thickness (girders included)
        For Each oGirder In oPanel.cScantlings.colGirder
            With oGirder
                sectgr = .WebHeight * (.WebThickness + oPanel.cScantlings.cPrimaryStiffeners.CorrosionThickness) + .FlangeWidth * (.FlangeThickness + oPanel.cScantlings.cPrimaryStiffeners.CorrosionThickness)
            End With
            eqplth = eqplth + Divide(sectgr, larg)
        Next oGirder
        
        'participation
        'eqplth = eqplth * oPanel.cGeometry.Participation
        
        'panel equivalent section
        AI = eqplth * larg
        SumAi = SumAi + AI
nextpan:
    Next oPanel
    
    GetCrossSectionGross = Round(SumAi, 5)
    
    Exit Function
GetCrossSectionGrossErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modFunctions: Function GetCrossSectionGross")
End Function

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    Me.Caption = "Geometric Properties - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
   
    txtGravityCenterNet = getGravityCenterNet
    txtGravityCenterGross = getGravityCenterGross
    txtNeutralAxisNet = getNeutralAxisNet
    txtNeutralAxisGross = getNeutralAxisGross
    txtInertiaYYNet = getInertiaYYNet
    txtInertiaYYGross = getInertiaYYGross
    txtSectionModulusNet = GetSectionModulusNet
    txtSectionModulusGross = GetSectionModulusGross
    txtCrossSectionNet = GetCrossSectionNet
    txtCrossSectionGross = GetCrossSectionGross
End Sub


