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
unit tdpe.lib.jansen.robot;

interface

Uses tdpe.lib.particle.group, tdpe.lib.particle.box.solid, tdpe.lib.particle.spring.restriction,
  tdpe.lib.engine, tdpe.lib.render, FMX.Graphics,
  tdpe.lib.particle.circle.solid, tdpe.lib.particle.abstractparticle,
  tdpe.lib.particle.wheel, tdpe.lib.jansen.body, tdpe.lib.jansen.motor, tdpe.lib.jansen.leg,
  System.UITypes, System.Types, System.UIConsts;

type
  TRobot = class(TGroup)
  private
    abody: Tbody;
    amotor: Tmotor;
    direction: integer;
    powerLevel: Double;
    powered: Boolean;
    legsVisible: Boolean;
    legLA: TLeg;
    legRA: TLeg;
    legLB: TLeg;
    legRB: TLeg;
    legLC: TLeg;
    legRC: TLeg;
    TEngine: TEngine;
    connLA, connRA, connLB, connRB, connLC, connRC, connLAA, connRAA, connLBB, connRBB, connLCC, connRCC: TSpringRestriction;
  public
    Constructor Create(aRenderer: TAbstractRenderer; anTEngine: TEngine; px: Double; py: Double; scale: Double; power: Double; xFactor, yFactor : double); Reintroduce;
    function px(): Double;
    function py(): Double;
    procedure run();
    procedure togglePower();
    procedure toggleDirection();
    procedure toggleLegs();
    procedure stiffness(s: Double);
    Destructor Destroy(); override;
  end;

implementation

uses tdpe.lib.particle.abstract.collection, SysUtils;

{ TRobot }

constructor TRobot.Create(aRenderer: TAbstractRenderer; anTEngine: TEngine; px: Double; py: Double; scale: Double; power: Double; xFactor, yFactor : double);
begin
  inherited Create(true);
  // legs
  TEngine := anTEngine;
  legLA := TLeg.Create(aRenderer, px * xFactor, py * yFactor, -1, scale, 2, 0, 1, 0, 1);
  legLA.SetStyle(claBlack, 2, claYellow);
  legLA.DrawTrace(true);
  legRA := TLeg.Create(aRenderer, px * xFactor, py * yFactor, 1, scale, 2, 0, 1, 0, 1);
  legRA.SetStyle(claBlack, 2, claYellow);
  legRA.DrawTrace(true);
  legLB := TLeg.Create(aRenderer, px * xFactor, py * yFactor, -1, scale, 2, 0, 1, 0, 1);
  legLB.SetStyle(claBlack, 2, claGreen);
  legRB := TLeg.Create(aRenderer, px * xFactor, py * yFactor, 1, scale, 2, 0, 1, 0, 1);
  legRB.SetStyle(claBlack, 2, claGreen);
  legLC := TLeg.Create(aRenderer, px * xFactor, py * yFactor, -1, scale, 2, 0, 1, 0, 1);
  legLC.SetStyle(claBlack, 2, claGray);
  legRC := TLeg.Create(aRenderer, px * xFactor, py * yFactor, 1, scale, 2, 0, 1, 0, 1);
  legRC.SetStyle(claBlack, 2, claGray);

  // body
  abody := Tbody.Create(aRenderer, legLA.fix, legRA.fix, 30 * scale, 2, 0, 1);

  // motor
  amotor := Tmotor.Create(aRenderer, anTEngine, abody.center, 8 * scale, 0);

  // connect the body to the legs
  connLA := TSpringRestriction.Create(abody.left, legLA.fix, 1);
  connLA.SetRenderer(aRenderer);
  connRA := TSpringRestriction.Create(abody.right, legRA.fix, 1);
  connRA.SetRenderer(aRenderer);
  connLB := TSpringRestriction.Create(abody.left, legLB.fix, 1);
  connLB.SetRenderer(aRenderer);
  connRB := TSpringRestriction.Create(abody.right, legRB.fix, 1);
  connRB.SetRenderer(aRenderer);
  connLC := TSpringRestriction.Create(abody.left, legLC.fix, 1);
  connLC.SetRenderer(aRenderer);
  connRC := TSpringRestriction.Create(abody.right, legRC.fix, 1);
  connRC.SetRenderer(aRenderer);

  // connect the legs to the motor
  legLA.cam.Position(amotor.rimA.Position);
  legRA.cam.Position(amotor.rimA.Position);
  connLAA := TSpringRestriction.Create(legLA.cam, amotor.rimA(), 1);
  connLAA.SetRenderer(aRenderer);
  connRAA := TSpringRestriction.Create(legRA.cam, amotor.rimA(), 1);
  connRAA.SetRenderer(aRenderer);

  legLB.cam.Position(amotor.rimB.Position);
  legRB.cam.Position(amotor.rimB.Position);
  connLBB := TSpringRestriction.Create(legLB.cam, amotor.rimB, 1);
  connLBB.SetRenderer(aRenderer);
  connRBB := TSpringRestriction.Create(legRB.cam, amotor.rimB, 1);
  connRBB.SetRenderer(aRenderer);

  legLC.cam.Position(amotor.rimC.Position);
  legRC.cam.Position(amotor.rimC.Position);
  connLCC := TSpringRestriction.Create(legLC.cam, amotor.rimC, 1);
  connLCC.SetRenderer(aRenderer);
  connRCC := TSpringRestriction.Create(legRC.cam, amotor.rimC, 1);
  connRCC.SetRenderer(aRenderer);

  // add to the engine
  addComposite(legLA);
  addComposite(legRA);
  addComposite(legLB);
  addComposite(legRB);
  addComposite(legLC);
  addComposite(legRC);

  addComposite(abody);
  addComposite(amotor);

  addRestriction(connLA);
  addRestriction(connRA);
  addRestriction(connLB);
  addRestriction(connRB);
  addRestriction(connLC);
  addRestriction(connRC);

  addRestriction(connLAA);
  addRestriction(connRAA);
  addRestriction(connLBB);
  addRestriction(connRBB);
  addRestriction(connLCC);
  addRestriction(connRCC);

  direction := -1;
  powerLevel := power;

  powered := true;
  legsVisible := true;

end;

destructor TRobot.Destroy;
begin
  FreeAndNil(abody);
  FreeAndNil(amotor);
  FreeAndNil(legLA);
  FreeAndNil(legRA);
  FreeAndNil(legLB);
  FreeAndNil(legRB);
  FreeAndNil(legLC);
  FreeAndNil(legRC);
  FreeAndNil(connLA);
  FreeAndNil(connRA);
  FreeAndNil(connLB);
  FreeAndNil(connRB);
  FreeAndNil(connLC);
  FreeAndNil(connRC);
  FreeAndNil(connLAA);
  FreeAndNil(connRAA);
  FreeAndNil(connLBB);
  FreeAndNil(connRBB);
  FreeAndNil(connLCC);
  FreeAndNil(connRCC);
  inherited;
end;

function TRobot.px: Double;
begin
  result := abody.center.px;
end;

function TRobot.py: Double;
begin
  result := abody.center.py;
end;

procedure TRobot.run;
begin
  amotor.run();
end;

procedure TRobot.stiffness(s: Double);
var
  i, j: integer;
  sp: TSpringRestriction;
begin
  // top level Restrictions in the group
  for i := 0 to Restrictions.Count - 1 do
  begin
    sp := TSpringRestriction(Restrictions.Items[i]);
    sp.stiffness(s);
  end;

  for j := 0 to composites.Count - 1 do
  begin
    for i := 0 to TAbstractCollection(composites.Items[j]).Restrictions.Count - 1 do
    begin
      sp := TSpringRestriction(TAbstractCollection(composites.Items[j]).Restrictions[i]);
      sp.stiffness(s);
    end;

  end;

end;

procedure TRobot.toggleDirection;
begin
  direction := direction * -1;
  amotor.power := powerLevel * direction;
end;

procedure TRobot.toggleLegs;
begin
  legsVisible := not legsVisible;
  if (not legsVisible) then
  begin
    legLA.visible := false;
    legRA.visible := false;
    legLB.visible := false;
    legRB.visible := false;
  end
  else
  begin
    legLA.visible := true;
    legRA.visible := true;
    legLB.visible := true;
    legRB.visible := true;
  end;
end;

procedure TRobot.togglePower;
begin
  powered := not powered;
  if (powered) then
  begin
    amotor.power := powerLevel * direction;
    stiffness(1);
    TEngine.damping := 0.99;
  end
  else
  begin
    amotor.power := 0;
    stiffness(0.2);
    TEngine.damping := 0.35;
  end;
end;

end.
