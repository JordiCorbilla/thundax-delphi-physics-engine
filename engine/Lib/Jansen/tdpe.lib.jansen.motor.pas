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
unit tdpe.lib.jansen.motor;

interface

Uses tdpe.lib.particle.group, tdpe.lib.particle.box.solid, tdpe.lib.particle.spring.restriction,
  tdpe.lib.engine, tdpe.lib.render, FMX.Graphics,
  tdpe.lib.particle.circle.solid, tdpe.lib.particle.abstractparticle,
  tdpe.lib.particle.wheel, tdpe.lib.particle.pattern.composite, System.UITypes, System.Types, System.UIConsts;

type
  TMotor = class(TComposite)
  private
    wheel: TWheelParticle;
    radius: Double;
    _rimA: TSolidCircle;
    _rimB: TSolidCircle;
    _rimC: TSolidCircle;
    ONE_THIRD: Double;
    FRenderer: TAbstractRenderer;
    axle: TSpringRestriction;
    // color: integer;
    procedure SetPower(const Value: Double);
    function GetPower(): Double;
  public
    Constructor Create(aRenderer: TAbstractRenderer; anTEngine: TEngine; attach: TAbstractParticle; radius: Double; color: Integer); Reintroduce; overload;
    property Power: Double read GetPower write SetPower;
    function rimA: TAbstractParticle;
    function rimB: TAbstractParticle;
    function rimC: TAbstractParticle;
    Procedure Init; override;
    Procedure Paint; override;
    procedure Run;
    Destructor Destroy(); override;
  end;

implementation

uses tdpe.lib.particle.abstract.collection, SysUtils;

{ TMotor }

constructor TMotor.Create(aRenderer: TAbstractRenderer; anTEngine: TEngine; attach: TAbstractParticle; radius: Double; color: Integer);
var
  theta: Double;
begin
  inherited Create;
  FRenderer := aRenderer;
  ONE_THIRD := (PI * 2) / 3;

  wheel := TWheelParticle.Create(anTEngine, attach.px, attach.py - 0.01, radius, false);
  wheel.SetRenderer(aRenderer);
  wheel.Visible := false;
  axle := TSpringRestriction.Create(wheel, attach);
  axle.SetRenderer(aRenderer);
  theta := wheel.radian;
  _rimA := TSolidCircle.Create(-radius * sin(theta) + wheel.px, radius * cos(theta) + wheel.py, 2, true);
  _rimA.SetRenderer(aRenderer);
  theta := theta + ONE_THIRD;
  _rimB := TSolidCircle.Create(-radius * sin(theta) + wheel.px, radius * cos(theta) + wheel.py, 2, true);
  _rimB.SetRenderer(aRenderer);
  theta := theta + ONE_THIRD;
  _rimC := TSolidCircle.Create(-radius * sin(theta) + wheel.px, radius * cos(theta) + wheel.py, 2, true);
  _rimC.SetRenderer(aRenderer);

  wheel.collidable := false;
  _rimA.collidable := false;
  _rimB.collidable := false;
  _rimC.collidable := false;

  addParticle(_rimA);
  addParticle(_rimB);
  addParticle(_rimC);
  addParticle(wheel);
  addRestriction(axle);
  self.radius := radius;
end;

function TMotor.rimA(): TAbstractParticle;
begin
  result := _rimA;
end;

function TMotor.rimB: TAbstractParticle;
begin
  result := _rimB;
end;

function TMotor.rimC: TAbstractParticle;
begin
  result := _rimC;
end;

procedure TMotor.Run();
var
  theta: Double;
begin
  // align the rim particle based on the wheel rotation
  theta := wheel.radian;
  _rimA.px := -radius * sin(theta) + wheel.px;
  _rimA.py := radius * cos(theta) + wheel.py;

  theta := theta + ONE_THIRD;
  _rimB.px := -radius * sin(theta) + wheel.px;
  _rimB.py := radius * cos(theta) + wheel.py;

  theta := theta + ONE_THIRD;
  _rimC.px := -radius * sin(theta) + wheel.px;
  _rimC.py := radius * cos(theta) + wheel.py;
end;

procedure TMotor.SetPower(const Value: Double);
begin
  wheel.speed := Value;
end;

destructor TMotor.Destroy;
begin
  FreeAndNil(wheel);
  FreeAndNil(_rimA);
  FreeAndNil(_rimB);
  FreeAndNil(_rimC);
  FreeAndNil(axle);
  inherited;
end;

function TMotor.GetPower: Double;
begin
  result := wheel.speed;
end;

procedure TMotor.Init;
begin
  inherited;

end;

procedure TMotor.Paint;
begin
  inherited;
  FRenderer.Line(wheel.px, wheel.py, _rimA.px, _rimA.py, claFuchsia, 2);
  FRenderer.Line(wheel.px, wheel.py, _rimB.px, _rimB.py, claFuchsia, 2);
  FRenderer.Line(wheel.px, wheel.py, _rimC.px, _rimC.py, claFuchsia, 2);
end;

end.
