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
unit tdpe.lib.math.rotator;

interface

uses
  tdpe.lib.vector, tdpe.lib.math.types;

type
  TRotator = class
  private
    FAngle: Double;
    Fposition: TRPoint;
    Fdirection: IVector;
    FPoint: TRPoint;
    procedure Setposition(const Value: TRPoint);
    procedure Setdirection(const Value: IVector);
  public
    property direction: IVector read Fdirection write Setdirection;
    property position: TRPoint read Fposition write Setposition;
    constructor create(const point: TRPoint; const angle: Double);
    destructor Destroy(); override;
    function RotateBy(const Value: Double): TRPoint; overload;
    function RotateBy(const Value: Double; const angle: Double): TRPoint; overload;
  end;

implementation

uses
  SysUtils;

{ TRotator }

constructor TRotator.create(const point: TRPoint; const angle: Double);
begin
  FPoint := TRPoint.create(point.x, point.y);
  Setposition(FPoint);
  Setdirection(TVector.new.SetPoint(1, 0));
  FAngle := angle;
  Fdirection.ResetAngle;
  Fdirection.TurnAngle(FAngle);
end;

destructor TRotator.Destroy;
begin
  Fdirection := nil;
  FreeAndNil(FPoint);
  inherited;
end;

function TRotator.RotateBy(const Value, angle: Double): TRPoint;
begin
  FAngle := angle;
  Fdirection.ResetAngle;
  Fdirection.TurnAngle(FAngle);
  result := RotateBy(Value);
end;

procedure TRotator.Setdirection(const Value: IVector);
begin
  Fdirection := Value;
end;

procedure TRotator.Setposition(const Value: TRPoint);
begin
  Fposition := Value;
end;

function TRotator.RotateBy(const Value: Double): TRPoint;
begin
  Fdirection.x := Value;
  Fdirection.y := 0;
  Fdirection.TurnAngle(FAngle);
  Fposition.Add(Fdirection.x, Fdirection.y);
  result := Fposition;
end;

end.
