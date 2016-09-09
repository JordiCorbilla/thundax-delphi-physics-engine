(*
 * Copyright (c) 2010-2012 Thundax Delphi Physics Engine
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * * Neither the name of 'TDPE' nor the names of its contributors
 *   may be used to endorse or promote products derived from this software
 *   without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)
unit tdpe.lib.render;

interface

uses
  tdpe.lib.vector, FMX.Graphics, tdpe.lib.styles, tdpe.lib.styles.contract, System.UITypes, System.Types;

Type

  TAbstractRenderer = Class
  public
    procedure DrawArrow(const Source, Target: IVector); Virtual; Abstract;
    Procedure Box(const xcenter, ycenter, Width, height, Rotate: Double; const color: TColor);
    Procedure Circle(const xcenter, ycenter, Radius, Rotate: Double; const style: IStyle); Virtual; Abstract;
    procedure CircleApproximation(const xcenter, ycenter, Radius, Rotate: Double; const style: IStyle); Virtual; Abstract;
    Procedure Line(const x1, y1, x2, y2: Double; const color: TColor; const penWidth: integer); virtual; abstract;
    Procedure Text(const x, y: Double; const Text: String; const color: TColor); Virtual; Abstract;
    Procedure FilledBox(const x, y, x1, y1, x2, y2, x3, y3: Double; const color: TColor); virtual; abstract;
    procedure FilledTriangle(const x, y, x1, y1, x2, y2: Double; const color: TColor; const penWidth: integer); virtual; abstract;
    Procedure DrawPoint(const x, y: Double; const color: TColor); virtual; abstract;
  end;

implementation

uses Math, SysUtils, tdpe.lib.Math.rotator, tdpe.lib.Math.types;

{ TAbstractRenderer }

procedure TAbstractRenderer.Box(const xcenter, ycenter, Width, height, Rotate: Double; const color: TColor);
var
  rotator: TRotator;
  p1, p2, p3, p4: TRPoint;
begin
  p1 := TRPoint.Create(xcenter, ycenter);
  p2 := TRPoint.Create(xcenter, ycenter);
  p3 := TRPoint.Create(xcenter, ycenter);
  p4 := TRPoint.Create(xcenter, ycenter);
  rotator := TRotator.Create(p1, DegToRad(Rotate + 180));
  try
    rotator.RotateBy(Width / 2);
    p1.SetTo(rotator.RotateBy(height / 2, DegToRad(Rotate + 90)));
    p2.SetTo(rotator.RotateBy(Width, DegToRad(Rotate)));
    p3.SetTo(rotator.RotateBy(height, DegToRad(Rotate - 90)));
    p4.SetTo(rotator.RotateBy(Width, DegToRad(Rotate + 180)));
    FilledBox(p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, p4.x, p4.y, color);
  finally
    FreeAndNil(rotator);
    FreeAndNil(p1);
    FreeAndNil(p2);
    FreeAndNil(p3);
    FreeAndNil(p4);
  end;
end;

end.
