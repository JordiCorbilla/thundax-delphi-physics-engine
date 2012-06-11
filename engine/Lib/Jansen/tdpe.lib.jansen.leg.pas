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
unit tdpe.lib.jansen.leg;

interface

Uses tdpe.lib.particle.group, tdpe.lib.particle.spring.restriction,
  tdpe.lib.engine, tdpe.lib.render, Graphics,
  tdpe.lib.particle.circle, tdpe.lib.particle.abstractparticle, tdpe.lib.particle.pattern.composite;

type
  TLeg = class(TComposite)
  private
    pa: TCircleParticle;
    pb: TCircleParticle;
    pc: TCircleParticle;
    pd: TCircleParticle;
    pe: TCircleParticle;
    pf: TCircleParticle;
    ph: TCircleParticle;
    lineColor: Integer;
    lineWeight: Integer;
    fillColor: TColor;
    _visible: Boolean;
    _DrawTrace: Boolean;
    cAB, cBC, cCA, CCD, CAE, cde, cDF, CEF, cBH, cEH: TSpringRestriction;
    FRenderer: TAbstractRenderer;
    function Getcam: TCircleParticle;
    procedure Setcam(const Value: TCircleParticle);
    procedure Setvisible(const Value: Boolean);
  public
    Constructor Create(aRenderer: TAbstractRenderer; px: Double; py: Double; orientation: Integer; scale: Double; lineWeight: Double; lineColor: Integer; lineAlpha: Double; fillColor: Integer;
      fillAlpha: Double); reintroduce; overload;
    property cam: TCircleParticle read Getcam write Setcam;
    function fix: TCircleParticle;
    property visible: Boolean read _visible write Setvisible;
    procedure Paint; override;
    procedure SetStyle(lineColor: TColor; lineWeight: Integer; fillColor: TColor);
    procedure DrawTrace(Value: Boolean);
    Destructor Destroy(); override;
  end;

implementation

uses tdpe.lib.particle.abstract.collection, SysUtils;

{ TLeg }

constructor TLeg.Create(aRenderer: TAbstractRenderer; px: Double; py: Double; orientation: Integer; scale: Double; lineWeight: Double; lineColor: Integer; lineAlpha: Double; fillColor: Integer;
  fillAlpha: Double);
var
  os: Double;
begin
  inherited Create;
  FRenderer := aRenderer;
  _DrawTrace := false;
  // top triangle -- pa is the attach point to the body
  os := orientation * scale;
  pa := TCircleParticle.Create(px + 31 * os, py - 8 * scale, 5, false, 1, 0.3, 0, clred);
  pa.SetRenderer(aRenderer);
  pb := TCircleParticle.Create(px + 25 * os, py - 37 * scale, 5, false);
  pb.SetRenderer(aRenderer);
  pc := TCircleParticle.Create(px + 60 * os, py - 15 * scale, 5, false);
  pc.SetRenderer(aRenderer);

  // bottom triangle particles -- pf is the foot
  pd := TCircleParticle.Create(px + 72 * os, py + 12 * scale, 5, false);
  pd.SetRenderer(aRenderer);
  pe := TCircleParticle.Create(px + 43 * os, py + 19 * scale, 5, false);
  pe.SetRenderer(aRenderer);
  pf := TCircleParticle.Create(px + 54 * os, py + 61 * scale, 2, false);
  pf.SetRenderer(aRenderer);

  // strut attach point particle
  ph := TCircleParticle.Create(px, py, 3, false, 1, 0.3, 0, clyellow);
  ph.SetRenderer(aRenderer);
  ph.visible := false;

  // top triangle Restrictions
  cAB := TSpringRestriction.Create(pa, pb, 1);
  cAB.SetRenderer(aRenderer);
  cBC := TSpringRestriction.Create(pb, pc, 1);
  cBC.SetRenderer(aRenderer);
  cCA := TSpringRestriction.Create(pc, pa, 1);
  cCA.SetRenderer(aRenderer);

  // middle leg Restrictions
  CCD := TSpringRestriction.Create(pc, pd, 1);
  CCD.SetRenderer(aRenderer);
  CAE := TSpringRestriction.Create(pa, pe, 1);
  CAE.SetRenderer(aRenderer);

  // bottom leg Restrictions
  cde := TSpringRestriction.Create(pd, pe, 1);
  cde.SetRenderer(aRenderer);
  cDF := TSpringRestriction.Create(pd, pf, 1);
  cDF.SetRenderer(aRenderer);
  CEF := TSpringRestriction.Create(pe, pf, 1);
  CEF.SetRenderer(aRenderer);

  // cam Restrictions
  cBH := TSpringRestriction.Create(pb, ph, 1);
  cBH.SetRenderer(aRenderer);
  cEH := TSpringRestriction.Create(pe, ph, 1);
  cEH.SetRenderer(aRenderer);

  addParticle(pa);
  addParticle(pb);
  addParticle(pc);
  addParticle(pd);
  addParticle(pe);
  addParticle(pf);
  addParticle(ph);

  addRestriction(cAB);
  addRestriction(cBC);
  addRestriction(cCA);
  addRestriction(CCD);
  addRestriction(CAE);
  addRestriction(cde);
  addRestriction(cDF);
  addRestriction(CEF);
  addRestriction(cBH);
  addRestriction(cEH);

  // for added efficiency, only test the feet (pf) for collision. these
  // selective tweaks should always be considered for best performance.
  pa.collidable := false;
  pb.collidable := false;
  pc.collidable := false;
  pd.collidable := false;
  pe.collidable := false;
  ph.collidable := false;

  _visible := true;
end;

function TLeg.Getcam(): TCircleParticle;
begin
  result := ph;
end;

procedure TLeg.Paint;
begin
  inherited;
  FRenderer.FilledTriangle(pa.px, pa.py, pb.px, pb.py, pc.px, pc.py, Self.fillColor, Self.lineWeight);
  FRenderer.FilledTriangle(pd.px, pd.py, pe.px, pe.py, pf.px, pf.py, Self.fillColor, Self.lineWeight);
  if _DrawTrace then
    FRenderer.DrawPoint(pf.px, pf.py, clLime);
end;

function TLeg.fix(): TCircleParticle;
begin
  result := pa;
end;

procedure TLeg.Setcam(const Value: TCircleParticle);
begin
  ph := Value;
end;

procedure TLeg.SetStyle(lineColor: TColor; lineWeight: Integer; fillColor: TColor);
begin
  Self.lineColor := lineColor;
  Self.lineWeight := lineWeight;
  Self.fillColor := fillColor;
end;

procedure TLeg.Setvisible(const Value: Boolean);
begin
  _visible := Value;
end;

destructor TLeg.Destroy;
begin
  FreeAndNil(pa);
  FreeAndNil(pb);
  FreeAndNil(pc);
  FreeAndNil(pd);
  FreeAndNil(pe);
  FreeAndNil(pf);
  FreeAndNil(ph);
  FreeAndNil(cAB);
  FreeAndNil(cBC);
  FreeAndNil(cCA);
  FreeAndNil(CCD);
  FreeAndNil(CAE);
  FreeAndNil(cde);
  FreeAndNil(cDF);
  FreeAndNil(CEF);
  FreeAndNil(cBH);
  FreeAndNil(cEH);
  inherited;
end;

procedure TLeg.DrawTrace(Value: Boolean);
begin
  _DrawTrace := Value;
end;

end.
