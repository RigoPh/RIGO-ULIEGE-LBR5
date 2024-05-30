VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "TGrid"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Private m_Handle As Long

Friend Property Get Grid() As MSHFlexGrid
    Set Grid = ObjectFromPtr(m_Handle)
End Property

Friend Property Let Grid(New_Grid As MSHFlexGrid)
    m_Handle = PtrFromObject(New_Grid)
End Property

Friend Property Set Grid(New_Grid As MSHFlexGrid)
    m_Handle = PtrFromObject(New_Grid)
End Property

