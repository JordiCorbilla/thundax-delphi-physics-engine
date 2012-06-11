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
unit tdpe.lib.jansen.body;

interface

Uses tdpe.lib.particle.group,
  tdpe.lib.particle.spring.restriction,
  tdpe.lib.engine, tdpe.lib.render, Graphics,
  tdpe.lib.particle.circle, tdpe.lib.particle.abstractparticle,
  tdpe.lib.particle.pattern.composite;

type
  TBody = class(TComposite)
  private
    top: TAbstractParticle;
    rgt: TAbstractParticle;
    lft: TAbstractParticle;
    bot: TAbstractParticle;
    ctr: TAbstractParticle;
    tr, rb, bl, lt, ct, cr, cb, cl: TSpringRestriction;
  public
    Constructor Create(aRenderer: TAbstractRenderer; left: TAbstractParticle; right: TAbstractParticle; height: double; lineWeight: double; lineColor: integer; lineAlpha: double); Reintroduce;
        overload;
    function center: TAbstractParticle;
    function left: TAbstractParticle;
    function right: TAbstractParticle;
    Destructor Destroy(); override;
  end;

implementation

uses tdpe.lib.particle.abstract.collection, SysUtils;

{ TBody }

constructor TBody.Create(aRenderer: TAbstractRenderer; left: TAbstractParticle; right: TAbstractParticle; height: double; lineWeight: double; lineColor: integer; lineAlpha: double);
var
  cpx, cpy: double;
begin
  inherited Create;
  cpx := (right.px + left.px) / 2;
  cpy := right.py;

  rgt := TCircleParticle.Create(right.px, right.py, 1, false);
  rgt.SetRenderer(aRenderer);
  rgt.visible := false;
  lft := TCircleParticle.Create(left.px, left.py, 1, false);
  lft.SetRenderer(aRenderer);
  lft.visible := false;

  ctr := TCircleParticle.Create(cpx, cpy, 1, false);
  ctr.SetRenderer(aRenderer);
  ctr.visible := false;
  top := TCircleParticle.Create(cpx, cpy - height / 2, 1, false);
  top.SetRenderer(aRenderer);
  top.visible := false;
  bot := TCircleParticle.Create(cpx, cpy + height / 2, 1, false);
  bot.SetRenderer(aRenderer);
  bot.visible := false;

  // outer Restrictions
  tr := TSpringRestriction.Create(top, rgt, 1);
  tr.SetRenderer(aRenderer);
  tr.visible := false;
  rb := TSpringRestriction.Create(rgt, bot, 1);
  rb.SetRenderer(aRenderer);
  rb.visible := false;
  bl := TSpringRestriction.Create(bot, lft, 1);
  bl.SetRenderer(aRenderer);
  bl.visible := false;
  lt := TSpringRestriction.Create(lft, top, 1);
  lt.SetRenderer(aRenderer);
  lt.visible := false;

  // inner constrainst
  ct := TSpringRestriction.Create(top, center, 1);
  ct.SetRenderer(aRenderer);
  ct.visible := false;
  cr := TSpringRestriction.Create(rgt, center, 1);
  cr.SetRenderer(aRenderer);
  cb := TSpringRestriction.Create(bot, center, 1);
  cb.SetRenderer(aRenderer);
  cb.visible := false;
  cl := TSpringRestriction.Create(lft, center, 1);
  cl.SetRenderer(aRenderer);

  ctr.collidable := false;
  top.collidable := false;
  rgt.collidable := false;
  bot.collidable := false;
  lft.collidable := false;

  addParticle(ctr);
  addParticle(top);
  addParticle(rgt);
  addParticle(bot);
  addParticle(lft);

  addRestriction(tr);
  addRestriction(rb);
  addRestriction(bl);
  addRestriction(lt);

  addRestriction(ct);
  addRestriction(cr);
  addRestriction(cb);
  addRestriction(cl);
end;

function TBody.left(): TAbstractParticle;
begin
  result := lft;
end;

function TBody.center(): TAbstractParticle;
begin
  result := ctr;
end;

function TBody.right(): TAbstractParticle;
begin
  result := rgt;
end;

destructor TBody.Destroy;
begin
  FreeAndNil(top);
  FreeAndNil(rgt);
  FreeAndNil(lft);
  FreeAndNil(bot);
  FreeAndNil(ctr);
  FreeAndNil(tr);
  FreeAndNil(rb);
  FreeAndNil(bl);
  FreeAndNil(lt);
  FreeAndNil(ct);
  FreeAndNil(cr);
  FreeAndNil(cb);
  FreeAndNil(cl);
  inherited;
end;

end.
