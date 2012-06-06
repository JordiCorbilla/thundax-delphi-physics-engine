object fBalls: TfBalls
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Thundax Ball Demo'
  ClientHeight = 716
  ClientWidth = 1164
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object tmr1: TTimer
    Interval = 1
    OnTimer = tmr1Timer
    Left = 40
    Top = 32
  end
end
