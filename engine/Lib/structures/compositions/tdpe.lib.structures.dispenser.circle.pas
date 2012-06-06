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
unit tdpe.lib.structures.dispenser.circle;

interface

uses
  tdpe.lib.particle.group, Contnrs, Generics.Collections, tdpe.lib.render,
  Graphics, tdpe.lib.engine, tdpe.lib.particle.circle.solid, tdpe.lib.writer.contract;

type
  TCircleDispenser = class(TGroup)
  private
    FCircles: TObjectList<TSolidCircle>;
    FRenderer: TAbstractRenderer;
    FProductColor: TColor;
    Fposx, Fposy: integer;
    FLog: IWriter;
  public
    Constructor Create(aRenderer: TAbstractRenderer; aEngine: TEngine; px, py: integer; color: TColor); Reintroduce;
    procedure setLog(const value: IWriter);
    destructor Destroy(); override;
    procedure AddCircle();
    function isInside(x, y: integer; var pos: integer): boolean;
    procedure Activate(i: integer);
    procedure Deactivate();
    procedure Move(x, y: integer);
  end;

implementation

uses
  tdpe.lib.colour.helper, sysUtils;

{ TCircleDispenser }

procedure TCircleDispenser.Activate(i: integer);
begin
  FCircles[i].StartDrag := true;
end;

procedure TCircleDispenser.AddCircle;
var
  newCircle: TSolidCircle;
begin
  if FCircles.Count < 100 then
  begin
    newCircle := TSolidCircle.Create(Fposx, Fposy, 20, false, 1, 0.3, 0.2, 1, 0.2, TRandomColor.getRandomColor);
    newCircle.setLog(FLog);
    newCircle.SetRenderer(FRenderer);
    AddParticle(newCircle);
    FCircles.Add(newCircle);
  end;
end;

constructor TCircleDispenser.Create(aRenderer: TAbstractRenderer; aEngine: TEngine; px, py: integer; color: TColor);
begin
  inherited Create(true);
  FRenderer := aRenderer;
  FProductColor := color;
  Fposx := px;
  Fposy := py;
  FCircles := TObjectList<TSolidCircle>.Create;
end;

procedure TCircleDispenser.Deactivate;
var
  circle: TSolidCircle;
begin
  for circle in FCircles do
    circle.StartDrag := false;
end;

destructor TCircleDispenser.Destroy;
begin
  FreeAndNil(FCircles);
  inherited;
end;

function TCircleDispenser.isInside(x, y: integer; var pos: integer): boolean;
var
  i: integer;
  bFound: boolean;
begin
  i := 0;
  bFound := false;
  while (i < FCircles.Count) and (not bFound) do
  begin
    bFound := FCircles[i].isInside(x, y);
    pos := i;
    Inc(i);
  end;
  result := bFound;
end;

procedure TCircleDispenser.Move(x, y: integer);
var
  circle: TSolidCircle;
begin
  for circle in FCircles do
  begin
    if circle.StartDrag then
      circle.Move(x, y);
  end;
end;

procedure TCircleDispenser.setLog(const value: IWriter);
begin
  FLog := value;
end;

end.
