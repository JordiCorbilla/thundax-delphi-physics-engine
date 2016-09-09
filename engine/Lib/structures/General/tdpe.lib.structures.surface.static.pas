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
unit tdpe.lib.structures.surface.static;

interface

Uses tdpe.lib.particle.group, tdpe.lib.particle.box.solid,
  tdpe.lib.particle.circle, tdpe.lib.particle.spring.restriction, tdpe.lib.engine,
  tdpe.lib.render, tdpe.lib.writer.contract, tdpe.lib.particle.circle.solid;

Type
  TSurfaces = class(TGroup)
  Private
    Floor: TSolidBox;
    RampRight: TSolidBox;
    FloorLeftDown: TSolidBox;
    FloorRightTop: TSolidBox;
    RampLeft: TSolidBox;
    RampLeftTop: TSolidBox;
    SpinningCircle1: TSolidCircle;
    SpinningCircle2: TSolidCircle;
    SpinningCircle3: TSolidCircle;
    SpinningCircle4: TSolidCircle;
    FLog: IWriter;
    FRender: TAbstractRenderer;
  Public
    Constructor Create(render: TAbstractRenderer; aEngine: TEngine; Log: IWriter); reintroduce; Virtual;
    procedure AddSurface();
    procedure setLog(const value: IWriter);
    destructor Destroy; override;
  End;

implementation

uses
  FMX.Graphics, SysUtils, tdpe.lib.colour.helper, System.UITypes, System.Types, System.UIConsts;

{ TSurfaces }

procedure TSurfaces.AddSurface;
begin
  Floor := TSolidBox.Create(410, 550, 360, 20, 0, true, 1, 0.3, 0.3, 0, claBlue);
  Floor.setLog(FLog);
  Floor.SetRenderer(FRender);
  addParticle(Floor);
end;

constructor TSurfaces.Create(render: TAbstractRenderer; aEngine: TEngine; Log: IWriter);
var
  rampx, rampy : double;
begin
  Inherited Create(False);
  FRender := render;
  FLog := Log;

  rampx := 200;
  rampy := 650;

  Floor := TSolidBox.Create(rampx+370, rampy-100, 360, 10, 0, true, 1, 0.3, 0.3, 0, claFuchsia);
  Floor.setLog(FLog);
  Floor.SetRenderer(render);
  addParticle(Floor);

  RampRight := TSolidBox.Create(rampx+700, rampy-185, 350, 10, -0.5, true, 1, 0.3, 0.3, 0, claFuchsia);
  RampRight.setLog(FLog);
  RampRight.SetRenderer(render);
  addParticle(RampRight);

  FloorLeftDown := TSolidBox.Create(rampx+250, rampy, 750, 10, 0, true, 1, 0.3, 0.3, 0, claFuchsia);
  FloorLeftDown.setLog(FLog);
  FloorLeftDown.SetRenderer(render);
  addParticle(FloorLeftDown);

  FloorRightTop := TSolidBox.Create(rampx+975, rampy-268, 250, 10, 0, true, 1, 0.3, 0.3, 0, claFuchsia);
  FloorRightTop.setLog(FLog);
  FloorRightTop.SetRenderer(render);
  addParticle(FloorRightTop);

  RampLeft := TSolidBox.Create(rampx+170, rampy-110, 50, 10, 0.5, true, 1, 0.3, 0.3, 0, claFuchsia);
  RampLeft.setLog(FLog);
  RampLeft.SetRenderer(render);
  addParticle(RampLeft);

  RampLeftTop := TSolidBox.Create(rampx-165, rampy-24, 100, 10, 0.5, true, 1, 0.3, 0.3, 0, claFuchsia);
  RampLeftTop.setLog(FLog);
  RampLeftTop.SetRenderer(render);
  addParticle(RampLeftTop);

  SpinningCircle1 := TSolidCircle.Create(700, 400, 40, true, 10, 0, 0, 0, 0.01, TRandomColor.getRandomColor());
  SpinningCircle1.setLog(FLog);
  SpinningCircle1.SetRenderer(render);
  addParticle(SpinningCircle1);

  SpinningCircle2 := TSolidCircle.Create(800, 180, 40, true, 10, 0, 0, 0, -0.01, TRandomColor.getRandomColor());
  SpinningCircle2.setLog(FLog);
  SpinningCircle2.SetRenderer(render);
  addParticle(SpinningCircle2);

  SpinningCircle3 := TSolidCircle.Create(900, 600, 60, true, 10, 0, 0, 0, -0.05, TRandomColor.getRandomColor());
  SpinningCircle3.setLog(FLog);
  SpinningCircle3.SetRenderer(render);
  addParticle(SpinningCircle3);

  SpinningCircle4 := TSolidCircle.Create(950, 350, 40, true, 10, 0, 0, 0, -0.01, TRandomColor.getRandomColor());
  SpinningCircle4.setLog(FLog);
  SpinningCircle4.SetRenderer(render);
  addParticle(SpinningCircle4);
end;

destructor TSurfaces.Destroy;
begin
  FreeAndNil(Floor);
  FreeAndNil(RampRight);
  FreeAndNil(FloorLeftDown);
  FreeAndNil(FloorRightTop);
  FreeAndNil(RampLeft);
  FreeAndNil(RampLeftTop);
  FreeAndNil(SpinningCircle1);
  FreeAndNil(SpinningCircle2);
  FreeAndNil(SpinningCircle3);
  FreeAndNil(SpinningCircle4);
  inherited;
end;

procedure TSurfaces.setLog(const value: IWriter);
begin
  FLog := value;
end;

end.
