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
unit tdpe.lib.automation.product;

interface

Uses tdpe.lib.particle.group, tdpe.lib.particle.circle.solid, tdpe.lib.particle.spring.restriction,
  tdpe.lib.engine, tdpe.lib.render, Contnrs, Generics.Collections, Graphics;

type
  TProduct = class(TGroup)
  private
    FCircles: TObjectList<TSolidCircle>;
    FRenderer: TAbstractRenderer;
    FProductColor: TColor;
    Fpos: integer;
  public
    Constructor Create(aRenderer: TAbstractRenderer; aEngine: TEngine; px: integer; color: TColor); Reintroduce;
    destructor Destroy(); override;
    procedure AddProduct();
  end;

implementation

uses tdpe.lib.particle.abstract.collection, SysUtils;

{ TProduct }

procedure TProduct.AddProduct;
var
  rawMaterial: TSolidCircle;
begin
  if FCircles.Count < 300 then
  begin
    rawMaterial := TSolidCircle.Create(Fpos + Random(30), 10, 3, false, 1, 0.3, 0.2, 0, 0, FProductColor);
    rawMaterial.SetRenderer(FRenderer);
    AddParticle(rawMaterial);
    FCircles.Add(rawMaterial);
  end;
end;

constructor TProduct.Create(aRenderer: TAbstractRenderer; aEngine: TEngine; px: integer; color: TColor);
begin
  inherited Create(True);
  FRenderer := aRenderer;
  FProductColor := color;
  Fpos := px;
  FCircles := TObjectList<TSolidCircle>.Create;
end;

destructor TProduct.Destroy;
begin
  FreeAndNil(FCircles);
  inherited;
end;

end.
