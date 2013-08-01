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
unit tdpe.lib.math.types;

interface

type
  TRPoint = class
  private
    Fx: double;
    Fy: double;
    procedure Setx(const Value: double);
    procedure Sety(const Value: double);
  public
    property x: double read Fx write Setx;
    property y: double read Fy write Sety;
    constructor Create(x, y: double);
    procedure Add(x, y : double);
    procedure SetTo(point : TRPoint);
  end;

implementation

{ TRPoint }

procedure TRPoint.Add(x, y: double);
begin
  Fx := Fx + x;
  Fy := Fy + y;
end;

constructor TRPoint.Create(x, y: double);
begin
  Setx(x);
  Sety(y);
end;

procedure TRPoint.SetTo(point: TRPoint);
begin
  Setx(point.x);
  Sety(point.y);
end;

procedure TRPoint.Setx(const Value: double);
begin
  Fx := Value;
end;

procedure TRPoint.Sety(const Value: double);
begin
  Fy := Value;
end;

end.
