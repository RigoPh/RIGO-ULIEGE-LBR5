Attribute VB_Name = "modPrint"
Public Sub Print1_(ByRef pic As PictureBox)
    On Error GoTo PrintErr
    Dim picture_aspect As Double, printer_aspect As Double
    Dim Wid As Double, hgt As Double
    Dim x As Double, y As Double
    Dim XP As Double, YP As Double
    Dim bufferBackColor As ColorConstants
    bufferBackColor = Project.Item(ActiveProject).cDisplaySettings.ColorScreen
    
'    On Error Resume Next
    With Project.Item(ActiveProject).frmProject.dlgCommonDialog
        .DialogTitle = "Print"
        .CancelError = True
        .flags = cdlPDReturnDC + cdlPDNoPageNums + cdlPDHidePrintToFile + cdlPDNoSelection
        .PrinterDefault = True
        .flags = .flags + pic
        .ShowPrinter
        Printer.Orientation = .Orientation
        If Err <> MSComDlg.cdlCancel Then
            picture_aspect = pic.Height / pic.Width
            printer_aspect = Printer.ScaleHeight / Printer.ScaleWidth
            If picture_aspect > printer_aspect Then
                hgt = Printer.ScaleHeight
                Wid = hgt / picture_aspect
            Else
                Wid = Printer.ScaleWidth
                hgt = Wid * picture_aspect
            End If
            XP = Printer.ScaleLeft + (Printer.ScaleWidth - Wid) / 2
            YP = Printer.ScaleTop + (Printer.ScaleHeight - hgt) / 2
            
            Project.Item(ActiveProject).cDisplaySettings.ColorScreen = vbWhite
            Draw ActiveProject
            ' Draw Border
            Dim rct As RECT
            rct.bottom = pic.Height
            rct.Top = pic.Top
            rct.Left = pic.Left
            rct.right = pic.Width
            Rectangle pic.hdc, _
                    rct.Left, rct.Top, rct.right, rct.bottom
            Printer.PaintPicture pic.Image, XP, YP, Wid, hgt
            Printer.EndDoc
            Project.Item(ActiveProject).cDisplaySettings.ColorScreen = bufferBackColor
            Draw ActiveProject
        End If
    End With
PrintCanceled:
Exit Sub
PrintErr:
    'MsgBox "Printer error!", vbCritical + vbOKOnly
    Project.Item(ActiveProject).cDisplaySettings.ColorScreen = bufferBackColor
    Draw ActiveProject
    Call RaiseError(MyUnhandledError, Err.Description)
End Sub

Public Sub Print_(ByVal pic As PictureBox)
    On Error GoTo PrintErr
    Dim PicRatio As Double
    Dim printerWidth As Double
    Dim printerHeight As Double
    Dim printerRatio As Double
    Dim printerPicWidth As Double
    Dim printerPicHeight As Double
    'pic.Picture = pic.Image
    ' Determine if picture should be printed in landscape or portrait
    ' and set the orientation.
    If pic.Height >= pic.Width Then
        Printer.Orientation = vbPRORPortrait ' Taller than wide.
    Else
        Printer.Orientation = vbPRORLandscape ' Wider than tall.
    End If
    ' Calculate device independent Width-to-Height ratio for picture.
    PicRatio = pic.Width / pic.Height
    ' Calculate the dimentions of the printable area in HiMetric.
    printerWidth = Printer.ScaleX(Printer.ScaleWidth, Printer.ScaleMode, vbHimetric)
    printerHeight = Printer.ScaleY(Printer.ScaleHeight, Printer.ScaleMode, vbHimetric)
    ' Calculate device independent Width to Height ratio for printer.
    printerRatio = printerWidth / printerHeight
    ' Scale the output to the printable area.
    If PicRatio >= printerRatio Then
        ' Scale picture to fit full width of printable area.
        printerPicWidth = Printer.ScaleX(printerWidth, vbHimetric, Printer.ScaleMode)
        printerPicHeight = Printer.ScaleY(printerWidth / PicRatio, vbHimetric, Printer.ScaleMode)
    Else
        ' Scale picture to fit full height of printable area.
        printerPicHeight = Printer.ScaleY(printerHeight, vbHimetric, Printer.ScaleMode)
        printerPicWidth = Printer.ScaleX(printerHeight * PicRatio, vbHimetric, Printer.ScaleMode)
    End If
    ' Print the picture using the PaintPicture method.
    'Exit Sub
    Dim rct As RECT
            rct.bottom = pic.Height
            rct.Top = pic.Top
            rct.Left = pic.Left
            rct.right = pic.Width
            Rectangle pic.hdc, _
                        rct.Left, rct.Top, rct.right, rct.bottom
                    
    Printer.PaintPicture pic.Image, 0, 0, printerPicWidth, printerPicHeight
    Printer.EndDoc
'PrintCanceled:
    Exit Sub
PrintErr:
    Call RaiseError(MyUnhandledError, Err.Description)
End Sub

