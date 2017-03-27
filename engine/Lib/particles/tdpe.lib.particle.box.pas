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
unit tdpe.lib.particle.box;

interface

uses
  tdpe.lib.particle.abstractparticle, tdpe.lib.math.helper, tdpe.lib.vector,
  tdpe.lib.math.interval, tdpe.lib.render, sysUtils, FMX.Graphics, System.UITypes, System.Types, System.UIConsts;

Type
  TRctTypeDouble = Array [0 .. 1] of Double;
  TRctTypeVector = Array [0 .. 1] of IVector;
  TRctTypeVector2 = Array [0 .. 3] of IVector;

  TBoxParticle = Class(TAbstractParticle)
  Private
    Fextents: TRctTypeDouble;
    Faxes: TRctTypeVector;
    Fradian: Double;
    FStartDrag: Boolean;
    function GetAngle: Double;
    function GetHeight: Double;
    function GetRadian: Double;
    function GetWidth: Double;
    procedure SetAngle(const Value: Double);
    procedure SetAxes(const Value: Double);
    procedure SetHeight(const Value: Double);
    procedure SetRadian(const Value: Double);
    procedure SetWidth(const Value: Double);
    procedure SetStartDrag(const Value: Boolean);
  Public
    Constructor Create(x, y, width, height, rotation: Double; Fixed: Boolean; Mass: Double = 1; Elasticity: Double = 0.3; Friction: Double = 0; color: TColor = claBlack); reintroduce;
    Destructor Destroy(); override;
    Procedure Init; Override;
    Procedure Paint; OVerride;
    Function GetProjection(AnAxis: IVector): TInterval;
    Procedure CleanUp; Override;
    Property Radian: Double read GetRadian Write SetRadian;
    Property Angle: Double read GetAngle Write SetAngle;
    Property width: Double read GetWidth Write SetWidth;
    Property height: Double read GetHeight Write SetHeight;
    Property Axes: TRctTypeVector read Faxes;
    Property Extents: TRctTypeDouble read Fextents Write Fextents;
    property StartDrag: Boolean read FStartDrag write SetStartDrag;
    function isInside(x, y: Integer): Boolean;
    procedure Move(x, y: Integer);
  end;

implementation

uses tdpe.lib.particle.abstractElement;

{ TBoxParticle }

procedure TBoxParticle.CleanUp;
begin
  inherited;

end;

constructor TBoxParticle.Create(x, y, width, height, rotation: Double; Fixed: Boolean; Mass: Double = 1; Elasticity: Double = 0.3; Friction: Double = 0; color: TColor = claBlack);
begin
  inherited Create(x, y, Mass, Elasticity, Friction, Fixed, color);
  Fextents[0] := width / 2;
  Fextents[1] := height / 2;
  Faxes[0] := TVector.New;
  Faxes[1] := TVector.New;
  SetAxes(rotation);
  Radian := rotation;
end;

destructor TBoxParticle.Destroy;
begin
  inherited;
end;

function TBoxParticle.GetAngle: Double;
begin
  Result := Radian * ONE_EIGHTY_OVER_PI;
end;

function TBoxParticle.GetHeight: Double;
begin
  Result := Fextents[1] * 2;
end;

function TBoxParticle.GetProjection(AnAxis: IVector): TInterval;
var
  radius: Double;
  c: Double;
begin
  radius := Extents[0] * Abs(AnAxis.InnerProduct(Axes[0])) + Extents[1] * Abs(AnAxis.InnerProduct(Axes[1]));
  c := Sample.InnerProduct(AnAxis);
  interval.min := c - radius;
  interval.max := c + radius;
  Result := interval;
end;

function TBoxParticle.GetRadian: Double;
begin
  Result := Fradian;
end;

function TBoxParticle.GetWidth: Double;
begin
  Result := Fextents[0] * 2;
end;

procedure TBoxParticle.Init;
begin
  Paint;
end;

function TBoxParticle.isInside(x, y: Integer): Boolean;
var
  res: Boolean;
begin
  res := False;
  if (x >= Self.px) and (x <= (Self.px + Self.width)) then
    if (y >= Self.py) and (y <= (Self.py + Self.height)) then
      res := True;
  Result := res;
end;

procedure TBoxParticle.Move(x, y: Integer);
begin
  current.x := x;
  current.y := y;
  if Fixed then
  begin
    Previous.x := x;
    Previous.y := y;
  end;
end;

procedure TBoxParticle.Paint;
var
  vec: IVector;
begin
  FRenderer.box(px, py, width, height, Angle, ParticleColor);
//  vec := TVector.New.SetPoint(Self.current.x + 20 * mtdres.x, current.y + 20 * mtdres.y);
//  Renderer.drawArrow(current, vec);
end;

procedure TBoxParticle.SetAngle(const Value: Double);
begin
  Radian := Value * PI_OVER_ONE_EIGHTY;
end;

procedure TBoxParticle.SetAxes(const Value: Double);
var
  s: Double;
  c: Double;
begin
  s := Sin(Value);
  c := Cos(Value);
  Axes[0].SetPoint(c, s);
  Axes[1].SetPoint(-s, c);
end;

procedure TBoxParticle.SetHeight(const Value: Double);
begin
  Fextents[1] := Value / 2;
end;

procedure TBoxParticle.SetRadian(const Value: Double);
begin
  Fradian := Value;
  SetAxes(Value);
end;

procedure TBoxParticle.SetStartDrag(const Value: Boolean);
begin
  FStartDrag := Value;
end;

procedure TBoxParticle.SetWidth(const Value: Double);
begin
  Fextents[0] := Value / 2;
end;

end.
