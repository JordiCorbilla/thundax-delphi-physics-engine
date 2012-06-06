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
unit tdpe.lib.vector;

interface

Uses Math;

Type
  ICustomVector<T> = interface
    function GetX(): T;
    procedure SetX(const value: T);
    property x: T read GetX write SetX;
    function GetY(): T;
    procedure SetY(const value: T);
    property y: T read GetY write SetY;
    function SetPoint(const px, py: T): ICustomVector<T>;
    procedure Clone(const vector: ICustomVector<T>);
    function InnerProduct(const vector: ICustomVector<T>): T;
    function OuterProduct(const vector: ICustomVector<T>): T;
    function Add(const vector: ICustomVector<T>; sameReference: Boolean = false): ICustomVector<T>;
    function Substract(const vector: ICustomVector<T>; sameReference: Boolean = false): ICustomVector<T>;
    function ScalarProduct(const value: Double; sameReference: Boolean = false): ICustomVector<T>;
    function Divide(const value: T): ICustomVector<T>;
    function Distance(const vector: ICustomVector<T>): Double;
    function Times(const vector: ICustomVector<T>): ICustomVector<T>;
    function Magnitude(): T;
    function Normalise(): ICustomVector<T>;
    function ToString(): String;
    procedure TurnAngle(const angle: T);
    procedure ResetAngle;
  end;

  IVector = ICustomVector<Double>;

  TVector = class(TInterfacedObject, IVector)
  private
    Fx: Double;
    Fy: Double;
    function GetX(): Double;
    procedure SetX(const value: Double);
    function GetY(): Double;
    procedure SetY(const value: Double);
    function PolarToCartesian(const R, Phi: Extended): IVector;
  Public
    function SetPoint(const px, py: Double): IVector;
    Procedure Clone(const vector: IVector);
    function InnerProduct(const vector: IVector): Double;
    function OuterProduct(const vector: IVector): Double;
    function Add(const vector: IVector; sameReference: Boolean = false): IVector;
    function Substract(const vector: IVector; sameReference: Boolean = false): IVector;
    function ScalarProduct(const value: Double; sameReference: Boolean = false): IVector;
    function Divide(const value: Double): IVector;
    function Distance(const vector: IVector): Double;
    function Times(const vector: IVector): IVector;
    function Magnitude(): Double;
    function Normalise(): IVector;
    function ToString(): String; override;
    procedure TurnAngle(const angle: Double);
    procedure ResetAngle;
    constructor Create(px: Double = 0; py: Double = 0);
    property x: Double read GetX write SetX;
    property y: Double read GetY write SetY;
    class function New: IVector;
  end;

implementation

uses SysUtils, tdpe.lib.math;

{ TVector }

procedure TVector.Clone(const vector: IVector);
begin
  SetX(vector.x);
  SetY(vector.y);
end;

constructor TVector.Create(px: Double = 0; py: Double = 0);
begin
  SetX(px);
  SetY(py);
end;

function TVector.OuterProduct(const vector: IVector): Double;
begin
  result := (Fx * vector.y) - (Fy * vector.x);
end;

function TVector.Distance(const vector: IVector): Double;
begin
  result := Substract(vector).Magnitude;
end;

function TVector.Divide(const value: Double): IVector;
var
  factor: Double;
begin
  factor := value;
  if TMathHelper.Compare(value, 0, '=') then
    factor := 0.0001;
  Fx := Fx / factor;
  Fy := Fy / factor;
  result := Self;
end;

function TVector.InnerProduct(const vector: IVector): Double;
begin
  result := (Fx * vector.x) + (Fy * vector.y);
end;

function TVector.GetX(): Double;
begin
  result := Fx;
end;

function TVector.GetY(): Double;
begin
  result := Fy;
end;

function TVector.Magnitude(): Double;
begin
  result := sqrt(Sqr(Fx) + Sqr(Fy));
end;

function TVector.Substract(const vector: IVector; sameReference: Boolean = false): IVector;
begin
  if not sameReference then
    result := TVector.New.SetPoint(Fx - vector.x, Fy - vector.y)
  else
  begin
    Fx := Fx - vector.x;
    Fy := Fy - vector.y;
    result := Self;
  end;
end;

function TVector.ScalarProduct(const value: Double; sameReference: Boolean = false): IVector;
begin
  if not sameReference then
    result := TVector.New.SetPoint(Fx * value, Fy * value)
  else
  begin
    Fx := Fx * value;
    Fy := Fy * value;
    result := Self;
  end;
end;

class function TVector.New(): IVector;
begin
  result := Create(0, 0);
end;

function TVector.Normalise(): IVector;
var
  m: Double;
begin
  m := Magnitude;
  if TMathHelper.Compare(m, 0, '=') then
    m := 0.0001;
  result := ScalarProduct(1 / m);
end;

function TVector.Add(const vector: IVector; sameReference: Boolean = false): IVector;
begin
  if not sameReference then
    result := TVector.New.SetPoint(Fx + vector.x, Fy + vector.y)
  else
  begin
    Fx := Fx + vector.x;
    Fy := Fy + vector.y;
    result := Self;
  end;
end;

function TVector.PolarToCartesian(const R, Phi: Extended): IVector;
var
  Sine, CoSine: Extended;
begin
  SinCos(Phi, Sine, CoSine);
  Fx := R * CoSine * -1;
  Fy := R * Sine * -1;
  result := Self;
end;

procedure TVector.ResetAngle();
begin
  Fx := Magnitude();
  Fy := 0;
end;

function TVector.SetPoint(const px, py: Double): IVector;
begin
  Fx := px;
  Fy := py;
  result := Self;
end;

procedure TVector.SetX(const value: Double);
begin
  Fx := value;
end;

procedure TVector.SetY(const value: Double);
begin
  Fy := value;
end;

function TVector.Times(const vector: IVector): IVector;
begin
  result := TVector.New.SetPoint(Fx * vector.x, Fy * vector.y);
end;

function TVector.ToString(): String;
begin
  result := 'Vector x:' + (FloatToStr(x) + ' y:' + FloatToStr(y));
end;

procedure TVector.TurnAngle(const angle: Double);
var
  n: Extended;
begin
  n := Magnitude;
  PolarToCartesian(n, angle);
end;

end.
